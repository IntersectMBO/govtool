'use strict';

// db-sync 13.x reads. Every query takes its inputs as bind parameters.
//
// Certificates are ordered by (tx_id, cert_index): a later row supersedes an
// earlier one, so "registered" means the latest registration is newer than
// the latest deregistration.

const ACCOUNT_SQL = `
WITH sa AS (
  SELECT id, view FROM stake_address WHERE hash_raw = $1
),
reg AS (
  SELECT r.tx_id, r.cert_index, b.epoch_no
  FROM stake_registration r
  JOIN tx ON tx.id = r.tx_id
  JOIN block b ON b.id = tx.block_id
  WHERE r.addr_id = (SELECT id FROM sa)
  ORDER BY r.tx_id DESC, r.cert_index DESC
  LIMIT 1
),
dereg AS (
  SELECT d.tx_id, d.cert_index
  FROM stake_deregistration d
  WHERE d.addr_id = (SELECT id FROM sa)
  ORDER BY d.tx_id DESC, d.cert_index DESC
  LIMIT 1
),
pool AS (
  SELECT d.tx_id, d.cert_index, d.active_epoch_no, ph.view AS pool_id
  FROM delegation d
  JOIN pool_hash ph ON ph.id = d.pool_hash_id
  WHERE d.addr_id = (SELECT id FROM sa)
  ORDER BY d.tx_id DESC, d.cert_index DESC
  LIMIT 1
),
vote AS (
  SELECT v.tx_id, v.cert_index, dh.raw, dh.view, dh.has_script
  FROM delegation_vote v
  JOIN drep_hash dh ON dh.id = v.drep_hash_id
  WHERE v.addr_id = (SELECT id FROM sa)
  ORDER BY v.tx_id DESC, v.cert_index DESC
  LIMIT 1
)
SELECT
  sa.view AS stake_address,
  reg.tx_id IS NOT NULL
    AND (dereg.tx_id IS NULL OR (reg.tx_id, reg.cert_index) > (dereg.tx_id, dereg.cert_index))
    AS registered,
  reg.epoch_no AS registered_epoch,
  CASE WHEN dereg.tx_id IS NULL OR (pool.tx_id, pool.cert_index) > (dereg.tx_id, dereg.cert_index)
    THEN pool.pool_id END AS pool_id,
  CASE WHEN dereg.tx_id IS NULL OR (pool.tx_id, pool.cert_index) > (dereg.tx_id, dereg.cert_index)
    THEN pool.active_epoch_no END AS active_epoch,
  CASE WHEN dereg.tx_id IS NULL OR (vote.tx_id, vote.cert_index) > (dereg.tx_id, dereg.cert_index)
    THEN vote.raw END AS drep_raw,
  CASE WHEN dereg.tx_id IS NULL OR (vote.tx_id, vote.cert_index) > (dereg.tx_id, dereg.cert_index)
    THEN vote.view END AS drep_view,
  CASE WHEN dereg.tx_id IS NULL OR (vote.tx_id, vote.cert_index) > (dereg.tx_id, dereg.cert_index)
    THEN vote.has_script END AS drep_has_script
FROM sa
LEFT JOIN reg ON true
LEFT JOIN dereg ON true
LEFT JOIN pool ON true
LEFT JOIN vote ON true
`;

// drep_registration: deposit > 0 registers, deposit < 0 retires, NULL is an
// update.
const DREP_SQL = `
WITH dh AS (
  SELECT id, raw, has_script FROM drep_hash WHERE raw = $1 AND has_script = $2
),
reg AS (
  SELECT r.tx_id, r.cert_index, b.epoch_no
  FROM drep_registration r
  JOIN tx ON tx.id = r.tx_id
  JOIN block b ON b.id = tx.block_id
  WHERE r.drep_hash_id = (SELECT id FROM dh) AND r.deposit > 0
  ORDER BY r.tx_id DESC, r.cert_index DESC
  LIMIT 1
),
ret AS (
  SELECT r.tx_id, r.cert_index
  FROM drep_registration r
  WHERE r.drep_hash_id = (SELECT id FROM dh) AND r.deposit < 0
  ORDER BY r.tx_id DESC, r.cert_index DESC
  LIMIT 1
),
distr AS (
  SELECT amount, active_until
  FROM drep_distr
  WHERE hash_id = (SELECT id FROM dh)
  ORDER BY epoch_no DESC
  LIMIT 1
)
SELECT
  dh.raw,
  dh.has_script,
  reg.epoch_no AS active_epoch,
  ret.tx_id IS NOT NULL
    AND (reg.tx_id IS NULL OR (ret.tx_id, ret.cert_index) > (reg.tx_id, reg.cert_index))
    AS retired,
  distr.amount::text AS amount,
  distr.active_until,
  (SELECT max(epoch_no) FROM block) AS current_epoch
FROM dh
LEFT JOIN reg ON true
LEFT JOIN ret ON true
LEFT JOIN distr ON true
WHERE reg.tx_id IS NOT NULL OR ret.tx_id IS NOT NULL
`;

async function account(db, hashRaw) {
  const { rows } = await db.query(ACCOUNT_SQL, [hashRaw]);
  return rows[0] || null;
}

async function dRep(db, raw, hasScript) {
  const { rows } = await db.query(DREP_SQL, [raw, hasScript]);
  return rows[0] || null;
}

module.exports = { account, dRep, ACCOUNT_SQL, DREP_SQL };
