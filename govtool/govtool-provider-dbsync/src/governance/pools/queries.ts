/**
 * SQL for stake pools as governance voters.
 *
 * Stake snapshots come from `pool_stat`. In db-sync 13.x, `pool_stat` for
 * epoch N is the snapshot taken at the start of N: its `voting_power` is the
 * SPO distribution governance ratifies by during N (Koios serves the same
 * figure as `pool_voting_power_history` for N), and its `stake` becomes the
 * leader-schedule stake of N+1 (it equals `epoch_stake` for N+1). So the
 * active stake of the current epoch N is `pool_stat` for N-1.
 *
 * `pool_stat` carries no index but its primary key, and rows are written one
 * epoch at a time. The latest two epochs therefore sit within the last
 * 2 x count(pool_hash) ids (one row per pool per epoch at most), which turns
 * what would be a full scan into a primary-key range scan. The epoch filter
 * stays on as the correctness guard.
 */

/** Latest registration certificate and latest retirement per pool, and status. */
const POOL_BASE = `
  tip AS (
    SELECT epoch_no FROM block WHERE block_no IS NOT NULL ORDER BY id DESC LIMIT 1
  ),
  snap AS (
    SELECT epoch_no FROM pool_stat ORDER BY id DESC LIMIT 1
  ),
  recent AS (
    SELECT ps.pool_hash_id, ps.epoch_no, ps.stake, ps.voting_power
      FROM pool_stat ps
     WHERE ps.id > (SELECT coalesce(max(id), 0) FROM pool_stat) - 2 * (SELECT count(*) FROM pool_hash)
  ),
  lu AS (
    SELECT DISTINCT ON (pu.hash_id) pu.hash_id, pu.registered_tx_id, pu.cert_index, pu.pledge, pu.meta_id
      FROM pool_update pu
     ORDER BY pu.hash_id, pu.registered_tx_id DESC, pu.cert_index DESC
  ),
  lr AS (
    SELECT DISTINCT ON (pr.hash_id) pr.hash_id, pr.announced_tx_id, pr.cert_index, pr.retiring_epoch
      FROM pool_retire pr
     ORDER BY pr.hash_id, pr.announced_tx_id DESC, pr.cert_index DESC
  ),
  pools AS (
    SELECT ph.id, ph.hash_raw, lu.pledge, pmr.url AS meta_url, pmr.hash AS meta_hash,
           -- Retired once a retirement announced after the latest registration
           -- has reached its epoch. A re-registration cancels a pending one.
           coalesce(lr.announced_tx_id > lu.registered_tx_id
                    OR (lr.announced_tx_id = lu.registered_tx_id AND lr.cert_index > lu.cert_index), false)
             AND lr.retiring_epoch <= tip.epoch_no AS retired
      FROM pool_hash ph
      JOIN lu ON lu.hash_id = ph.id
      LEFT JOIN lr ON lr.hash_id = ph.id
      LEFT JOIN pool_metadata_ref pmr ON pmr.id = lu.meta_id
     CROSS JOIN tip
  )`;

/**
 * $1 pool hash hex or null, $2 limit, $3 offset, $4 true to include retired
 * pools (a single-pool read) or false (the listing: registered pools only).
 */
export const POOLS_SQL = `
  WITH ${POOL_BASE}
  SELECT encode(p.hash_raw, 'hex') AS hash, p.pledge::text AS pledge, p.retired,
         p.meta_url, encode(p.meta_hash, 'hex') AS meta_hash,
         (SELECT epoch_no FROM snap)::int AS snap_epoch,
         vp.voting_power::text AS voting_power,
         act.stake::text AS active_stake,
         EXISTS (SELECT 1 FROM recent r, tip WHERE r.epoch_no = tip.epoch_no - 1) AS active_known,
         count(*) OVER () AS total_count
    FROM pools p
    LEFT JOIN recent vp ON vp.pool_hash_id = p.id AND vp.epoch_no = (SELECT epoch_no FROM snap)
    LEFT JOIN recent act ON act.pool_hash_id = p.id AND act.epoch_no = (SELECT epoch_no - 1 FROM tip)
   WHERE ($4::boolean OR NOT p.retired)
     AND ($1::text IS NULL OR p.hash_raw = decode($1::text, 'hex'))
   ORDER BY vp.voting_power DESC NULLS LAST, p.hash_raw
   LIMIT $2 OFFSET $3`;

/** A pool known to have registered at least once, by hash. $1 pool hash hex. */
export const POOL_ID_SQL = `
  SELECT ph.id::text AS id
    FROM pool_hash ph
   WHERE ph.hash_raw = decode($1::text, 'hex')
     AND EXISTS (SELECT 1 FROM pool_update pu WHERE pu.hash_id = ph.id)`;

/**
 * A pool's effective vote on each action it voted on: a later vote on the same
 * action replaces an earlier one, so only the latest per action is served.
 * `voting_procedure` has no index on `pool_voter`, so this scans the table.
 * $1 pool_hash.id, $2 limit, $3 offset.
 */
export const POOL_VOTES_SQL = `
  WITH v AS (
    SELECT DISTINCT ON (vp.gov_action_proposal_id)
           vp.tx_id, vp.index, vp.gov_action_proposal_id, vp.vote, vp.voting_anchor_id
      FROM voting_procedure vp
     WHERE vp.pool_voter = $1::bigint AND vp.voter_role = 'SPO' AND vp.invalid IS NULL
     ORDER BY vp.gov_action_proposal_id, vp.tx_id DESC, vp.index DESC
  )
  SELECT v.vote::text AS vote, encode(t.hash, 'hex') AS tx_hash, v.index::int AS index,
         b.block_no::text AS block_no, b.epoch_no::int AS epoch_no, b.slot_no::text AS slot_no, b.time,
         va.url AS anchor_url, encode(va.data_hash, 'hex') AS anchor_hash,
         encode(gt.hash, 'hex') AS action_tx_hash, g.index::int AS action_index, g.type::text AS action_type,
         count(*) OVER () AS total_count
    FROM v
    JOIN tx t ON t.id = v.tx_id
    JOIN block b ON b.id = t.block_id
    JOIN gov_action_proposal g ON g.id = v.gov_action_proposal_id
    JOIN tx gt ON gt.id = g.tx_id
    LEFT JOIN voting_anchor va ON va.id = v.voting_anchor_id
   ORDER BY v.tx_id DESC, v.index DESC
   LIMIT $2 OFFSET $3`;
