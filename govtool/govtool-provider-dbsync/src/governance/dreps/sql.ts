/**
 * db-sync 13.x statements for the DRep area.
 *
 * Every value that comes from a caller is a bind parameter. The only text ever
 * spliced into a statement is an ORDER BY clause chosen from a fixed map in
 * this file, keyed by a value that has already been checked against a
 * whitelist; caller input never reaches it.
 *
 * Each statement opens with a comment tag (`dreps:list`, ...) so a test double
 * can route on it and so a slow statement is identifiable in pg_stat_activity.
 */

/** Tip epoch: the "current epoch" a DRep expiry is compared with. */
const TIP_EPOCH = `(SELECT epoch_no FROM block WHERE epoch_no IS NOT NULL ORDER BY id DESC LIMIT 1)`;

/**
 * The epoch of the newest DRep distribution db-sync has written. Read off the
 * primary key rather than max(epoch_no), which has no index of its own.
 */
const SNAPSHOT_EPOCH = `(SELECT epoch_no FROM drep_distr ORDER BY id DESC LIMIT 1)`;

/**
 * One row per credential that has ever registered as a DRep, with the facts
 * the directory filters and sorts on.
 *
 * Status, in order of precedence:
 *   retired   the latest registration/retirement certificate is a retirement.
 *   active    the DRep voted or sent a registration/update certificate in or
 *             after the snapshot epoch. Each of those sets the ledger expiry to
 *             at least the epoch it happened in plus drepActivity, so the DRep
 *             is not expired now, whatever the (older) snapshot says.
 *   by expiry the ledger's DRep expiry as db-sync records it
 *             (drep_distr.active_until at the newest snapshot):
 *             inactive when the tip epoch is past it.
 *   fallback  db-sync records the expiry only for DReps in the stake
 *             distribution, i.e. with at least one delegator. For the rest it
 *             holds no ledger expiry at all, so the status is the lower bound
 *             the ledger guarantees: last activity epoch + drepActivity. This
 *             is the one place activity is derived rather than read; see
 *             `expiry_known`, which is false for these rows so no expiryEpoch
 *             is ever emitted for them.
 *
 * $1 status[] | null, $2 kind[] | null, $3 search hash | null, $4 search is-script
 */
const DIRECTORY = `
  reg AS (
    SELECT DISTINCT ON (drep_hash_id) drep_hash_id, tx_id, cert_index
      FROM drep_registration
     WHERE deposit > 0
     ORDER BY drep_hash_id, tx_id DESC, cert_index DESC
  ),
  state AS (
    SELECT DISTINCT ON (drep_hash_id) drep_hash_id, deposit < 0 AS retired
      FROM drep_registration
     WHERE deposit IS NOT NULL
     ORDER BY drep_hash_id, tx_id DESC, cert_index DESC
  ),
  cur AS (
    SELECT DISTINCT ON (drep_hash_id) drep_hash_id, tx_id, voting_anchor_id
      FROM drep_registration
     WHERE deposit IS NULL OR deposit > 0
     ORDER BY drep_hash_id, tx_id DESC, cert_index DESC
  ),
  lastvote AS (
    SELECT drep_voter AS drep_hash_id, max(tx_id) AS tx_id
      FROM voting_procedure
     WHERE drep_voter IS NOT NULL
     GROUP BY drep_voter
  ),
  facts AS (
    SELECT dh.id, dh.raw, dh.has_script,
           reg.tx_id AS reg_tx, reg.cert_index AS reg_cert,
           state.retired,
           cur.voting_anchor_id,
           dd.amount, dd.active_until,
           ab.epoch_no AS activity_epoch,
           rb.epoch_no AS reg_epoch,
           COALESCE(ep.drep_activity, (SELECT drep_activity FROM epoch_param ORDER BY epoch_no DESC LIMIT 1)) AS drep_activity,
           t.tip_epoch, t.snap_epoch
      FROM reg
      JOIN drep_hash dh ON dh.id = reg.drep_hash_id
      JOIN tx rtx ON rtx.id = reg.tx_id
      JOIN block rb ON rb.id = rtx.block_id
      JOIN state ON state.drep_hash_id = reg.drep_hash_id
      JOIN cur ON cur.drep_hash_id = reg.drep_hash_id
      LEFT JOIN lastvote lv ON lv.drep_hash_id = reg.drep_hash_id
      JOIN tx atx ON atx.id = GREATEST(cur.tx_id, lv.tx_id)
      JOIN block ab ON ab.id = atx.block_id
      LEFT JOIN epoch_param ep ON ep.epoch_no = ab.epoch_no
      CROSS JOIN (SELECT ${TIP_EPOCH} AS tip_epoch, ${SNAPSHOT_EPOCH} AS snap_epoch) t
      LEFT JOIN drep_distr dd ON dd.hash_id = dh.id AND dd.epoch_no = t.snap_epoch
     WHERE dh.raw IS NOT NULL
       AND ($3::bytea IS NULL OR (dh.raw = $3::bytea AND dh.has_script = $4::boolean))
  ),
  directory AS (
    SELECT f.*,
           CASE
             WHEN f.retired THEN 'retired'
             WHEN f.activity_epoch >= f.snap_epoch THEN 'active'
             WHEN f.active_until IS NOT NULL THEN
               CASE WHEN f.tip_epoch > f.active_until THEN 'inactive' ELSE 'active' END
             WHEN f.activity_epoch + f.drep_activity >= f.tip_epoch THEN 'active'
             ELSE 'inactive'
           END AS status,
           (NOT f.retired AND f.active_until IS NOT NULL
             AND NOT COALESCE(f.activity_epoch >= f.snap_epoch, false)) AS expiry_known,
           CASE WHEN f.voting_anchor_id IS NULL THEN 'anonymous' ELSE 'drep' END AS kind,
           -- The ledger's distribution omits DReps with no delegated stake, so
           -- a DRep registered before the snapshot and absent from it has 0.
           -- One registered since has no snapshot figure yet: NULL.
           CASE WHEN f.amount IS NOT NULL THEN f.amount
                WHEN f.reg_epoch < f.snap_epoch THEN 0
           END AS power
      FROM facts f
  ),
  filtered AS (
    SELECT * FROM directory
     WHERE ($1::text[] IS NULL OR status = ANY($1::text[]))
       AND ($2::text[] IS NULL OR kind = ANY($2::text[]))
  )`;

/**
 * ORDER BY per sort key. `id` (db-sync's drep_hash id, unique) breaks ties so
 * an ordering is total and pages neither overlap nor skip.
 */
export const LIST_ORDER = {
  votingPower: 'power DESC NULLS LAST, id ASC',
  registrationDate: 'reg_tx DESC, reg_cert DESC, id ASC',
  random: 'random()',
} as const;
export type ListOrderKey = keyof typeof LIST_ORDER;

/** $5 limit, $6 offset. */
export const listSql = (order: ListOrderKey) => `/* dreps:list */
WITH ${DIRECTORY}
SELECT id, encode(raw, 'hex') AS hash, has_script, status, expiry_known, active_until,
       power AS amount, snap_epoch, count(*) OVER () AS total_count
  FROM filtered
 ORDER BY ${LIST_ORDER[order]}
 LIMIT $5 OFFSET $6`;

export const countSql = `/* dreps:count-filtered */
WITH ${DIRECTORY}
SELECT count(*) AS total_count FROM filtered`;

/** Directory-wide counters, read off the same status and kind as the listing. */
export const countsSql = `/* dreps:counts */
WITH ${DIRECTORY}
SELECT count(*) FILTER (WHERE status <> 'retired') AS registered,
       count(*) FILTER (WHERE status = 'active') AS active,
       count(*) FILTER (WHERE status = 'inactive') AS inactive,
       count(*) FILTER (WHERE status <> 'retired' AND kind = 'anonymous') AS anonymous
  FROM directory`;

/**
 * Registration facts for the DReps in $1 (drep_hash ids).
 *
 *   latest registration  the newest certificate with a positive deposit.
 *   latest update        the newest update certificate (NULL deposit) since it.
 *   retirement           the newest retirement, only while it is the latest
 *                        registration/retirement certificate.
 *   anchor               of the newest non-retirement certificate.
 */
export const detailsSql = `/* dreps:details */
WITH certs AS (
  SELECT r.id, r.drep_hash_id, r.tx_id, r.cert_index, r.deposit, r.voting_anchor_id
    FROM drep_registration r
   WHERE r.drep_hash_id = ANY($1::bigint[])
),
reg AS (
  SELECT DISTINCT ON (drep_hash_id) * FROM certs WHERE deposit > 0
   ORDER BY drep_hash_id, tx_id DESC, cert_index DESC
),
upd AS (
  SELECT DISTINCT ON (c.drep_hash_id) c.*
    FROM certs c JOIN reg ON reg.drep_hash_id = c.drep_hash_id
   WHERE c.deposit IS NULL AND (c.tx_id, c.cert_index) > (reg.tx_id, reg.cert_index)
   ORDER BY c.drep_hash_id, c.tx_id DESC, c.cert_index DESC
),
state AS (
  SELECT DISTINCT ON (drep_hash_id) * FROM certs WHERE deposit IS NOT NULL
   ORDER BY drep_hash_id, tx_id DESC, cert_index DESC
),
cur AS (
  SELECT DISTINCT ON (drep_hash_id) * FROM certs WHERE deposit IS NULL OR deposit > 0
   ORDER BY drep_hash_id, tx_id DESC, cert_index DESC
),
events AS (
  SELECT 'registration' AS event, drep_hash_id, id FROM reg
  UNION ALL SELECT 'update', drep_hash_id, id FROM upd
  UNION ALL SELECT 'retirement', drep_hash_id, id FROM state WHERE deposit < 0
)
SELECT e.event, e.drep_hash_id,
       encode(tx.hash, 'hex') AS tx_hash, c.cert_index, c.deposit,
       b.block_no, b.epoch_no, b.slot_no, b.time,
       va.url AS anchor_url, encode(va.data_hash, 'hex') AS anchor_hash,
       cva.url AS current_anchor_url, encode(cva.data_hash, 'hex') AS current_anchor_hash,
       (cur.voting_anchor_id IS NOT NULL) AS has_current_anchor
  FROM events e
  JOIN certs c ON c.id = e.id
  JOIN tx ON tx.id = c.tx_id
  JOIN block b ON b.id = tx.block_id
  LEFT JOIN voting_anchor va ON va.id = c.voting_anchor_id
  JOIN cur ON cur.drep_hash_id = e.drep_hash_id
  LEFT JOIN voting_anchor cva ON cva.id = cur.voting_anchor_id`;

/**
 * Live delegator counts for the DReps in $1.
 *
 * A stake credential delegates to a DRep when its newest vote delegation names
 * the DRep and nothing has cleared it since: no retirement of the DRep (a
 * retirement clears delegations and they do not return on re-registration) and
 * no stake deregistration. A retired DRep has none. DReps with no row have zero.
 *
 * Deliberately NOT "made after the newest registration": before protocol 10 a
 * delegation to a not-yet-registered DRep was legal, and preview holds
 * delegations placed ahead of the registration certificate in the same
 * transaction that the ledger still counts (Koios agrees).
 *
 * delegation_vote has no index on drep_hash_id or addr_id in stock db-sync, so
 * this reads the table twice (hash joins, no per-row scans).
 */
export const delegatorCountsSql = `/* dreps:delegator-counts */
WITH retire AS (
  SELECT drep_hash_id, tx_id, cert_index
    FROM drep_registration
   WHERE deposit < 0 AND drep_hash_id = ANY($1::bigint[])
),
state AS (
  SELECT DISTINCT ON (drep_hash_id) drep_hash_id, deposit < 0 AS retired
    FROM drep_registration
   WHERE deposit IS NOT NULL AND drep_hash_id = ANY($1::bigint[])
   ORDER BY drep_hash_id, tx_id DESC, cert_index DESC
),
cand AS (
  SELECT dv.addr_id, dv.drep_hash_id, dv.tx_id, dv.cert_index
    FROM delegation_vote dv
    JOIN state ON state.drep_hash_id = dv.drep_hash_id AND NOT state.retired
   WHERE NOT EXISTS (SELECT 1 FROM retire rt
                      WHERE rt.drep_hash_id = dv.drep_hash_id
                        AND (rt.tx_id, rt.cert_index) > (dv.tx_id, dv.cert_index))
)
SELECT c.drep_hash_id, count(*) AS delegators
  FROM cand c
 WHERE NOT EXISTS (SELECT 1 FROM delegation_vote l
                    WHERE l.addr_id = c.addr_id AND (l.tx_id, l.cert_index) > (c.tx_id, c.cert_index))
   AND NOT EXISTS (SELECT 1 FROM stake_deregistration sd
                    WHERE sd.addr_id = c.addr_id AND (sd.tx_id, sd.cert_index) > (c.tx_id, c.cert_index))
 GROUP BY c.drep_hash_id`;

/**
 * The vote listing for the DReps in $1: every action the DRep voted on, plus
 * every action that was votable while it was registered.
 *
 * Window: from the DRep's FIRST registration to its retirement (when the
 * latest registration/retirement certificate is one) or the tip epoch.
 *
 * An action is votable in epochs [submitted, lastVotable], where lastVotable
 * is the epoch before it was ratified or dropped, or the epoch it expired in
 * (db-sync records expired_epoch one epoch before dropped_epoch). Still-open
 * actions have no end. During the Conway bootstrap phase (protocol < 10) the
 * ledger rejects DRep votes on anything but an InfoAction, so a non-Info
 * action counts only if it was open in a post-bootstrap epoch of the window.
 *
 * Voted rows are always included, so voted is a subset of the listing and
 * activity can never read above 100 %.
 */
const VOTE_ROWS = `
  win AS (
    SELECT r.drep_hash_id, min(b.epoch_no) AS start_epoch
      FROM drep_registration r
      JOIN tx ON tx.id = r.tx_id
      JOIN block b ON b.id = tx.block_id
     WHERE r.drep_hash_id = ANY($1::bigint[]) AND r.deposit > 0
     GROUP BY r.drep_hash_id
  ),
  st AS (
    SELECT DISTINCT ON (r.drep_hash_id) r.drep_hash_id, r.deposit < 0 AS retired, b.epoch_no
      FROM drep_registration r
      JOIN tx ON tx.id = r.tx_id
      JOIN block b ON b.id = tx.block_id
     WHERE r.drep_hash_id = ANY($1::bigint[]) AND r.deposit IS NOT NULL
     ORDER BY r.drep_hash_id, r.tx_id DESC, r.cert_index DESC
  ),
  w AS (
    SELECT win.drep_hash_id, win.start_epoch,
           CASE WHEN st.retired THEN st.epoch_no ELSE ${TIP_EPOCH} END AS end_epoch,
           (SELECT COALESCE(min(epoch_no), 2147483647) FROM epoch_param WHERE protocol_major >= 10) AS bootstrap_end
      FROM win JOIN st ON st.drep_hash_id = win.drep_hash_id
  ),
  props AS (
    SELECT g.id, g.type, g.tx_id, g.index, g.voting_anchor_id, b.epoch_no AS sub_epoch,
           COALESCE(g.ratified_epoch - 1, g.dropped_epoch - 1, g.expired_epoch) AS last_epoch
      FROM gov_action_proposal g
      JOIN tx ON tx.id = g.tx_id
      JOIN block b ON b.id = tx.block_id
  ),
  votes AS (
    SELECT DISTINCT ON (vp.drep_voter, vp.gov_action_proposal_id)
           vp.drep_voter, vp.gov_action_proposal_id, vp.vote, vp.voting_anchor_id, vp.tx_id, vp.index
      FROM voting_procedure vp
     WHERE vp.drep_voter = ANY($1::bigint[]) AND vp.invalid IS NULL
     ORDER BY vp.drep_voter, vp.gov_action_proposal_id, vp.tx_id DESC, vp.index DESC
  ),
  rows AS (
    SELECT w.drep_hash_id, p.id AS proposal_id, p.type, p.tx_id AS proposal_tx_id, p.index AS proposal_index,
           p.voting_anchor_id AS proposal_anchor_id,
           v.vote, v.voting_anchor_id AS vote_anchor_id, v.tx_id AS vote_tx_id, v.index AS vote_index
      FROM w
     CROSS JOIN props p
      LEFT JOIN votes v ON v.drep_voter = w.drep_hash_id AND v.gov_action_proposal_id = p.id
     WHERE v.tx_id IS NOT NULL
        OR (p.sub_epoch <= w.end_epoch
            AND (p.last_epoch IS NULL OR p.last_epoch >= w.start_epoch)
            AND (p.type = 'InfoAction'
                 OR (w.end_epoch >= w.bootstrap_end
                     AND (p.last_epoch IS NULL OR p.last_epoch >= w.bootstrap_end))))
  )`;

export const activitySql = `/* dreps:activity */
WITH ${VOTE_ROWS}
SELECT drep_hash_id, count(*) AS votable, count(vote_tx_id) AS voted
  FROM rows
 GROUP BY drep_hash_id`;

export const VOTE_ORDER = {
  newest: 'COALESCE(r.vote_tx_id, r.proposal_tx_id) DESC, r.proposal_id DESC',
  oldest: 'COALESCE(r.vote_tx_id, r.proposal_tx_id) ASC, r.proposal_id ASC',
} as const;
export type VoteOrderKey = keyof typeof VOTE_ORDER;

/**
 * One page of a DRep's vote listing. $2 voted filter (null = both), $3 limit,
 * $4 offset.
 *
 * The title is the one piece of resolved metadata chain data may carry (a
 * governance action title on a DRep vote row). It is taken only from a
 * document db-sync fetched whose hash matches the on-chain anchor.
 */
export const votesSql = (order: VoteOrderKey) => `/* dreps:votes */
WITH ${VOTE_ROWS},
page AS (
  SELECT r.*, count(*) OVER () AS total_count
    FROM rows r
   WHERE $2::boolean IS NULL OR ((r.vote_tx_id IS NOT NULL) = $2::boolean)
   ORDER BY ${VOTE_ORDER[order]}
   LIMIT $3 OFFSET $4
)
SELECT r.type, encode(ptx.hash, 'hex') AS proposal_tx_hash, r.proposal_index,
       r.vote, encode(vtx.hash, 'hex') AS vote_tx_hash, r.vote_index,
       vb.block_no, vb.epoch_no, vb.slot_no, vb.time,
       va.url AS anchor_url, encode(va.data_hash, 'hex') AS anchor_hash,
       (SELECT gad.title
          FROM off_chain_vote_data ocvd
          JOIN voting_anchor pva ON pva.id = ocvd.voting_anchor_id
          JOIN off_chain_vote_gov_action_data gad ON gad.off_chain_vote_data_id = ocvd.id
         WHERE ocvd.voting_anchor_id = r.proposal_anchor_id AND ocvd.hash = pva.data_hash
         ORDER BY ocvd.id DESC LIMIT 1) AS title,
       r.total_count
  FROM page r
  JOIN tx ptx ON ptx.id = r.proposal_tx_id
  LEFT JOIN tx vtx ON vtx.id = r.vote_tx_id
  LEFT JOIN block vb ON vb.id = vtx.block_id
  LEFT JOIN voting_anchor va ON va.id = r.vote_anchor_id
 ORDER BY ${VOTE_ORDER[order]}`;

export const votesCountSql = `/* dreps:votes-count */
WITH ${VOTE_ROWS}
SELECT count(*) AS total_count FROM rows r
 WHERE $2::boolean IS NULL OR ((r.vote_tx_id IS NOT NULL) = $2::boolean)`;

/** The drep_hash row for a credential that has registered at least once. $1 hash, $2 is-script. */
export const resolveSql = `/* dreps:resolve */
SELECT dh.id
  FROM drep_hash dh
 WHERE dh.raw = $1::bytea AND dh.has_script = $2::boolean
   AND EXISTS (SELECT 1 FROM drep_registration r WHERE r.drep_hash_id = dh.id AND r.deposit > 0)`;

export const HISTORY_ORDER = {
  asc: 'r.tx_id ASC, r.cert_index ASC',
  desc: 'r.tx_id DESC, r.cert_index DESC',
} as const;
export type HistoryOrderKey = keyof typeof HISTORY_ORDER;

/**
 * The metadata-change feed: every registration and update certificate (each
 * sets the anchor), never retirements. $1 drep_hash id, $2 limit, $3 offset.
 */
export const historySql = (order: HistoryOrderKey) => `/* dreps:history */
SELECT encode(tx.hash, 'hex') AS tx_hash, r.cert_index, r.deposit,
       b.block_no, b.epoch_no, b.slot_no, b.time,
       va.url AS anchor_url, encode(va.data_hash, 'hex') AS anchor_hash,
       count(*) OVER () AS total_count
  FROM drep_registration r
  JOIN tx ON tx.id = r.tx_id
  JOIN block b ON b.id = tx.block_id
  LEFT JOIN voting_anchor va ON va.id = r.voting_anchor_id
 WHERE r.drep_hash_id = $1 AND (r.deposit IS NULL OR r.deposit > 0)
 ORDER BY ${HISTORY_ORDER[order]}
 LIMIT $2 OFFSET $3`;

export const historyCountSql = `/* dreps:history-count */
SELECT count(*) AS total_count FROM drep_registration r
 WHERE r.drep_hash_id = $1 AND (r.deposit IS NULL OR r.deposit > 0)`;
