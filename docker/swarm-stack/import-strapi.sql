-- Imports the proposal discussion forum's data from a Strapi database into
-- govtool-pdf-backend's schema.
--
-- Run against the pdf database, as a superuser, after the Strapi tables have
-- been restored into a schema named `strapi` in that same database. Runs in
-- one transaction: on any error nothing is written. Prints a summary.
--
--   psql -v ON_ERROR_STOP=1 -U postgres -d pdf -f import-strapi.sql
--
-- What it does:
-- - Keeps Strapi's ids for users, proposals, contents and comments, so old
--   links (/proposal_discussion/<id>) still resolve. It therefore refuses to
--   run unless the forum's own tables are empty.
-- - Maps governance action types by name, not by id: the two databases number
--   them differently. Strapi rows with no match are added.
-- - Turns Strapi's string references (proposal_id, user_id, master_id, ...)
--   into foreign keys and skips rows whose target no longer exists.
-- - Merges Strapi accounts that share a username (concurrent sign-ins created
--   some moments apart) into one, keeping every account's proposals, comments,
--   votes and reports. The new schema has one account per username.
-- - Enforces what the new schema adds: one vote per user, one active poll,
--   64-hex submission hashes, 56-character DRep ids. Counters (likes,
--   dislikes, comments, poll totals) are recomputed from the imported rows.
-- - Leaves out what the new backend does not keep: Strapi admin, roles,
--   permissions, files, auth challenges, proposal submissions, update
--   committee contents, and budget discussions (D166): BDs, their drafts,
--   polls, votes, lookups and comments. The 2025 budget proposals are a
--   static archive in the frontend.

\set ON_ERROR_STOP on
BEGIN;

-- ------------------------------------------------------------- preflight

DO $$
DECLARE
  t text;
  n bigint;
BEGIN
  IF to_regnamespace('strapi') IS NULL THEN
    RAISE EXCEPTION 'schema "strapi" not found: restore the Strapi tables into it first';
  END IF;
  FOREACH t IN ARRAY ARRAY['users', 'proposals', 'proposal_contents', 'comments',
                           'proposal_votes', 'polls', 'comments_reports'] LOOP
    EXECUTE format('SELECT count(*) FROM public.%I', t) INTO n;
    IF n > 0 THEN
      RAISE EXCEPTION 'public.% already has % rows; this import needs an empty forum', t, n;
    END IF;
  END LOOP;
END $$;

-- A Strapi string reference as an integer id, or NULL.
CREATE FUNCTION pg_temp.ref(v text) RETURNS integer LANGUAGE sql IMMUTABLE AS $$
  SELECT CASE WHEN btrim(v) ~ '^[0-9]{1,9}$' THEN btrim(v)::integer END
$$;

-- Strapi wrote UTC into `timestamp without time zone`.
CREATE FUNCTION pg_temp.ts(v timestamp) RETURNS timestamptz LANGUAGE sql IMMUTABLE AS $$
  SELECT coalesce(v AT TIME ZONE 'UTC', now())
$$;

CREATE TEMP TABLE import_report (step text, imported bigint, skipped bigint, note text) ON COMMIT DROP;

-- ------------------------------------------------------------- lookups
-- map_gat holds strapi_id -> pdf_id. Unmatched Strapi rows are inserted
-- with new ids first.

INSERT INTO governance_action_types (name, published_at, created_at, updated_at)
SELECT s.gov_action_type_name, now(), now(), now()
FROM strapi.governance_action_types s
WHERE s.gov_action_type_name IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM governance_action_types g WHERE lower(g.name) = lower(s.gov_action_type_name));
CREATE TEMP TABLE map_gat ON COMMIT DROP AS
SELECT s.id AS sid, g.id AS pid FROM strapi.governance_action_types s
JOIN governance_action_types g ON lower(g.name) = lower(s.gov_action_type_name);

-- ------------------------------------------------------------- users
-- map_user holds strapi_id -> pdf_id. Accounts sharing a username map to one
-- of them: the one with the most proposals, comments and votes, then one with
-- a GovTool username, then the oldest. Every user reference below goes
-- through it, so a merged account's content moves to the kept one.

CREATE TEMP TABLE user_refs ON COMMIT DROP AS
SELECT uid, count(*) AS n
FROM (
  SELECT pg_temp.ref(user_id) AS uid FROM strapi.proposals
  UNION ALL SELECT pg_temp.ref(user_id) FROM strapi.comments
  UNION ALL SELECT pg_temp.ref(user_id) FROM strapi.proposal_votes
  UNION ALL SELECT pg_temp.ref(user_id) FROM strapi.poll_votes
) r
WHERE uid IS NOT NULL
GROUP BY uid;

CREATE TEMP TABLE map_user ON COMMIT DROP AS
SELECT u.id AS sid,
       first_value(u.id) OVER (PARTITION BY lower(btrim(u.username))
                               ORDER BY coalesce(r.n, 0) DESC,
                                        coalesce(length(btrim(u.govtool_username)) BETWEEN 1 AND 30, false) DESC,
                                        u.id) AS pid
FROM strapi.up_users u
LEFT JOIN user_refs r ON r.uid = u.id
WHERE u.username IS NOT NULL AND length(btrim(u.username)) BETWEEN 1 AND 58;

-- A merged account is validated or blocked if any of its accounts was, and
-- takes the kept account's GovTool username, else the oldest one set.
INSERT INTO users (id, username, govtool_username, is_validated, blocked, created_at, updated_at)
SELECT m.pid, lower(btrim(min(u.username))),
       (array_agg(btrim(u.govtool_username) ORDER BY u.id = m.pid DESC, u.id)
          FILTER (WHERE length(btrim(u.govtool_username)) BETWEEN 1 AND 30))[1],
       bool_or(coalesce(u.is_validated, false)), bool_or(coalesce(u.blocked, false)),
       pg_temp.ts(min(u.created_at)), pg_temp.ts(max(u.updated_at))
FROM map_user m
JOIN strapi.up_users u ON u.id = m.sid
GROUP BY m.pid;
INSERT INTO import_report
SELECT 'users', (SELECT count(*) FROM users), (SELECT count(*) FROM strapi.up_users) - (SELECT count(*) FROM map_user),
       'skipped: missing or over-long username';
INSERT INTO import_report
SELECT 'users merged', (SELECT count(*) FROM map_user WHERE sid <> pid), NULL,
       'accounts sharing a username, merged with their content into one';

-- ------------------------------------------------------------- proposals

INSERT INTO proposals (id, user_id, created_at, updated_at)
SELECT p.id, m.pid, pg_temp.ts(p.created_at), pg_temp.ts(p.updated_at)
FROM strapi.proposals p
JOIN map_user m ON m.sid = pg_temp.ref(p.user_id);
INSERT INTO import_report
SELECT 'proposals', (SELECT count(*) FROM proposals), (SELECT count(*) FROM strapi.proposals) - (SELECT count(*) FROM proposals),
       'skipped: author not imported';

-- Each content's proposal: its proposal_id string, else Strapi's link table.
CREATE TEMP TABLE src_content ON COMMIT DROP AS
SELECT c.*,
       coalesce(pg_temp.ref(c.proposal_id),
                (SELECT l.proposal_id FROM strapi.proposal_contents_proposal_links l
                 WHERE l.proposal_content_id = c.id ORDER BY l.id LIMIT 1)) AS pid,
       (SELECT m.pid FROM map_gat m WHERE m.sid = pg_temp.ref(c.gov_action_type_id)) AS gat,
       (SELECT l.proposal_hard_fork_content_id FROM strapi.proposal_contents_proposal_hard_fork_content_links l
        WHERE l.proposal_content_id = c.id ORDER BY l.id LIMIT 1) AS hf
FROM strapi.proposal_contents c;
DELETE FROM src_content WHERE pid IS NULL OR gat IS NULL OR pid NOT IN (SELECT id FROM proposals);

-- One content per hard-fork row and per submission hash; later ones lose it.
UPDATE src_content s SET hf = NULL
FROM (SELECT id, row_number() OVER (PARTITION BY hf ORDER BY id) AS rn FROM src_content WHERE hf IS NOT NULL) d
WHERE d.id = s.id AND d.rn > 1;
UPDATE src_content SET hf = NULL WHERE hf NOT IN (SELECT id FROM strapi.proposal_hard_fork_contents);

INSERT INTO proposal_hard_fork_contents (id, previous_ga_hash, previous_ga_id, major, minor, created_at, updated_at)
SELECT h.id, h.previous_ga_hash, h.previous_ga_id, h.major, h.minor, pg_temp.ts(h.created_at), pg_temp.ts(h.updated_at)
FROM strapi.proposal_hard_fork_contents h WHERE h.id IN (SELECT hf FROM src_content);

INSERT INTO proposal_contents (id, proposal_id, user_id, gov_action_type_id, name, abstract, motivation, rationale,
                               rev_active, is_draft, submitted, is_locked, submission_tx_hash, submission_date,
                               hard_fork_content_id, created_at, updated_at)
SELECT c.id, c.pid, p.user_id, c.gat, left(coalesce(c.prop_name, ''), 80),
       coalesce(c.prop_abstract, ''), coalesce(c.prop_motivation, ''), coalesce(c.prop_rationale, ''),
       coalesce(c.prop_rev_active, false), coalesce(c.is_draft, false), coalesce(c.prop_submitted, false),
       coalesce(c.is_locked, false),
       CASE WHEN h.rn = 1 THEN h.hash END, c.prop_submission_date, c.hf,
       pg_temp.ts(c.created_at), pg_temp.ts(c.updated_at)
FROM src_content c
JOIN proposals p ON p.id = c.pid
LEFT JOIN (SELECT id, lower(btrim(prop_submission_tx_hash)) AS hash,
                  row_number() OVER (PARTITION BY lower(btrim(prop_submission_tx_hash)) ORDER BY id) AS rn
           FROM src_content WHERE lower(btrim(prop_submission_tx_hash)) ~ '^[0-9a-f]{64}$') h ON h.id = c.id;
INSERT INTO import_report
SELECT 'proposal_contents', (SELECT count(*) FROM proposal_contents),
       (SELECT count(*) FROM strapi.proposal_contents) - (SELECT count(*) FROM proposal_contents),
       'skipped: proposal or action type not found';

INSERT INTO proposal_constitution_contents (id, content_id, constitution_url, have_guardrails_script,
                                            guardrails_script_url, guardrails_script_hash, created_at, updated_at)
SELECT DISTINCT ON (l.proposal_content_id)
       k.id, l.proposal_content_id, left(k.prop_constitution_url, 2048), k.prop_have_guardrails_script,
       left(k.prop_guardrails_script_url, 2048), k.prop_guardrails_script_hash,
       pg_temp.ts(k.created_at), pg_temp.ts(k.updated_at)
FROM strapi.proposal_constitution_contents k
JOIN strapi.proposal_constitution_contents_proposal_content_links l ON l.proposal_constitution_content_id = k.id
WHERE l.proposal_content_id IN (SELECT id FROM proposal_contents)
ORDER BY l.proposal_content_id, k.id;

INSERT INTO proposal_links (content_id, position, link, text)
SELECT pc.entity_id, row_number() OVER (PARTITION BY pc.entity_id ORDER BY pc."order", pc.id) - 1,
       left(coalesce(l.prop_link, ''), 2048), left(l.prop_link_text, 255)
FROM strapi.proposal_contents_components pc
JOIN strapi.components_proposal_proposal_links l ON l.id = pc.component_id
WHERE pc.component_type = 'proposal.proposal-link' AND pc.entity_id IN (SELECT id FROM proposal_contents);

INSERT INTO proposal_withdrawals (content_id, position, receiving_address, amount)
SELECT pc.entity_id, row_number() OVER (PARTITION BY pc.entity_id ORDER BY pc."order", pc.id) - 1,
       left(w.prop_receiving_address, 200), w.prop_amount
FROM strapi.proposal_contents_components pc
JOIN strapi.components_proposal_proposal_withdrawals w ON w.id = pc.component_id
WHERE pc.component_type = 'proposal.proposal-withdrawals' AND pc.entity_id IN (SELECT id FROM proposal_contents);

-- Likes and dislikes: one per user and proposal, the latest wins.
INSERT INTO proposal_votes (id, proposal_id, user_id, vote_result, created_at, updated_at)
SELECT DISTINCT ON (pg_temp.ref(v.proposal_id), m.pid)
       v.id, pg_temp.ref(v.proposal_id), m.pid, v.vote_result, pg_temp.ts(v.created_at), pg_temp.ts(v.updated_at)
FROM strapi.proposal_votes v
JOIN map_user m ON m.sid = pg_temp.ref(v.user_id)
WHERE v.vote_result IS NOT NULL
  AND pg_temp.ref(v.proposal_id) IN (SELECT id FROM proposals)
ORDER BY pg_temp.ref(v.proposal_id), m.pid, v.updated_at DESC NULLS LAST, v.id DESC;
INSERT INTO import_report
SELECT 'proposal_votes', (SELECT count(*) FROM proposal_votes),
       (SELECT count(*) FROM strapi.proposal_votes) - (SELECT count(*) FROM proposal_votes),
       'skipped: duplicate vote, or proposal or user not found';

-- Polls: at most one active per proposal, the latest.
INSERT INTO polls (id, proposal_id, start_dt, is_active, created_at, updated_at)
SELECT p.id, pg_temp.ref(p.proposal_id), p.poll_start_dt AT TIME ZONE 'UTC',
       coalesce(p.is_poll_active AND p.id = (SELECT max(q.id) FROM strapi.polls q
                     WHERE pg_temp.ref(q.proposal_id) = pg_temp.ref(p.proposal_id) AND q.is_poll_active), false),
       pg_temp.ts(p.created_at), pg_temp.ts(p.updated_at)
FROM strapi.polls p
WHERE pg_temp.ref(p.proposal_id) IN (SELECT id FROM proposals);

INSERT INTO poll_votes (id, poll_id, user_id, vote_result, created_at, updated_at)
SELECT DISTINCT ON (pg_temp.ref(v.poll_id), m.pid)
       v.id, pg_temp.ref(v.poll_id), m.pid, v.vote_result, pg_temp.ts(v.created_at), pg_temp.ts(v.updated_at)
FROM strapi.poll_votes v
JOIN map_user m ON m.sid = pg_temp.ref(v.user_id)
WHERE v.vote_result IS NOT NULL
  AND pg_temp.ref(v.poll_id) IN (SELECT id FROM polls)
ORDER BY pg_temp.ref(v.poll_id), m.pid, v.updated_at DESC NULLS LAST, v.id DESC;
INSERT INTO import_report
SELECT 'polls / poll_votes', (SELECT count(*) FROM polls) + (SELECT count(*) FROM poll_votes),
       (SELECT count(*) FROM strapi.polls) + (SELECT count(*) FROM strapi.poll_votes)
         - (SELECT count(*) FROM polls) - (SELECT count(*) FROM poll_votes),
       'skipped: duplicate vote, or proposal, poll or user not found';

-- ------------------------------------------------------------- comments

-- Each comment's proposal, when imported. Budget discussion comments have
-- none and are left out (D166).
CREATE TEMP TABLE src_comment ON COMMIT DROP AS
SELECT c.*,
       CASE WHEN pg_temp.ref(c.proposal_id) IN (SELECT id FROM proposals) THEN pg_temp.ref(c.proposal_id) END AS prop,
       (SELECT m.pid FROM map_user m WHERE m.sid = pg_temp.ref(c.user_id)) AS uid
FROM strapi.comments c;
DELETE FROM src_comment WHERE prop IS NULL OR uid IS NULL;
-- A reply whose parent is gone, or sits on another proposal, becomes top-level.
ALTER TABLE src_comment ADD COLUMN parent integer;
UPDATE src_comment s SET parent = p.id
FROM src_comment p
WHERE p.id = pg_temp.ref(s.comment_parent_id) AND p.id <> s.id AND p.prop = s.prop;

INSERT INTO comments (id, proposal_id, parent_id, user_id, text, drep_id, created_at, updated_at)
SELECT c.id, c.prop, c.parent, c.uid, coalesce(c.comment_text, ''),
       CASE WHEN length(btrim(c.drep_id)) = 56 THEN btrim(c.drep_id) END,
       pg_temp.ts(c.created_at), pg_temp.ts(c.updated_at)
FROM src_comment c;
INSERT INTO import_report
SELECT 'comments', (SELECT count(*) FROM comments), (SELECT count(*) FROM strapi.comments) - (SELECT count(*) FROM comments),
       'skipped: budget discussion comment, or proposal or author not found';

INSERT INTO comments_reports (id, comment_id, reporter_id, moderator_id, moderation_status, hash,
                              published_at, created_at, updated_at)
SELECT DISTINCT ON (r.comment_id, r.reporter_id)
       r.id, r.comment_id, r.reporter_id, r.moderator_id, r.moderation_status,
       CASE WHEN r.hash ~ '^[A-Za-z0-9]{89}$' AND r.hash_rn = 1 THEN r.hash
            ELSE left(replace(gen_random_uuid()::text || gen_random_uuid()::text || gen_random_uuid()::text, '-', ''), 89) END,
       coalesce(r.published_at AT TIME ZONE 'UTC', pg_temp.ts(r.created_at)), pg_temp.ts(r.created_at), pg_temp.ts(r.updated_at)
FROM (
  SELECT x.*,
    (SELECT l.comment_id FROM strapi.comments_reports_comment_links l WHERE l.comments_report_id = x.id ORDER BY l.id LIMIT 1) AS comment_id,
    (SELECT m.pid FROM strapi.comments_reports_reporter_links l JOIN map_user m ON m.sid = l.user_id
      WHERE l.comments_report_id = x.id ORDER BY l.id LIMIT 1) AS reporter_id,
    (SELECT m.pid FROM strapi.comments_reports_moderator_links l JOIN map_user m ON m.sid = l.user_id
      WHERE l.comments_report_id = x.id ORDER BY l.id LIMIT 1) AS moderator_id,
    row_number() OVER (PARTITION BY x.hash ORDER BY x.id) AS hash_rn
  FROM strapi.comments_reports x
) r
WHERE r.comment_id IN (SELECT id FROM comments) AND r.reporter_id IN (SELECT id FROM users)
ORDER BY r.comment_id, r.reporter_id, r.id;
INSERT INTO import_report
SELECT 'comments_reports', (SELECT count(*) FROM comments_reports),
       (SELECT count(*) FROM strapi.comments_reports) - (SELECT count(*) FROM comments_reports),
       'skipped: duplicate, or comment or reporter not found';

-- ------------------------------------------------------------- counters

UPDATE proposals p SET
  likes = (SELECT count(*) FROM proposal_votes v WHERE v.proposal_id = p.id AND v.vote_result),
  dislikes = (SELECT count(*) FROM proposal_votes v WHERE v.proposal_id = p.id AND NOT v.vote_result),
  comments_number = (SELECT count(*) FROM comments c WHERE c.proposal_id = p.id);
UPDATE polls p SET
  yes = (SELECT count(*) FROM poll_votes v WHERE v.poll_id = p.id AND v.vote_result),
  no = (SELECT count(*) FROM poll_votes v WHERE v.poll_id = p.id AND NOT v.vote_result);

-- ------------------------------------------------------------- sequences

DO $$
DECLARE t text;
BEGIN
  FOREACH t IN ARRAY ARRAY['users', 'proposals', 'proposal_contents', 'proposal_links', 'proposal_withdrawals',
    'proposal_constitution_contents', 'proposal_hard_fork_contents', 'proposal_votes', 'polls', 'poll_votes',
    'comments', 'comments_reports', 'governance_action_types'] LOOP
    EXECUTE format('SELECT setval(pg_get_serial_sequence(%L, ''id''), coalesce((SELECT max(id) FROM %I), 1), (SELECT max(id) FROM %I) IS NOT NULL)', t, t, t);
  END LOOP;
END $$;

INSERT INTO import_report VALUES
  ('not imported', (SELECT count(*) FROM strapi.proposal_submitions), NULL, 'proposal_submitions (no equivalent)'),
  ('not imported', (SELECT count(*) FROM strapi.proposal_update_committee_contents), NULL, 'update committee contents (no equivalent)'),
  ('not imported', (SELECT count(*) FROM strapi.bds), NULL, 'budget discussion versions (D166)'),
  ('not imported', (SELECT count(*) FROM strapi.bd_polls) + (SELECT count(*) FROM strapi.bd_poll_votes), NULL,
   'budget discussion polls and votes (D166)'),
  ('not imported', (SELECT count(*) FROM strapi.bd_drafts), NULL, 'budget discussion drafts (D166)');

SELECT step, imported, skipped, note FROM import_report;

COMMIT;
