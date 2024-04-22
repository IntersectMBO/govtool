WITH DRepDistr AS (
  SELECT
    *,
    ROW_NUMBER() OVER (PARTITION BY drep_hash.id ORDER BY drep_distr.epoch_no DESC) AS rn
  FROM
    drep_distr
    JOIN drep_hash ON drep_hash.id = drep_distr.hash_id
),
DRepActivity AS (
  SELECT
    drep_activity AS drep_activity,
    epoch_no AS epoch_no
  FROM
    epoch_param
  WHERE
    epoch_no IS NOT NULL
  ORDER BY
    epoch_no DESC
  LIMIT 1
)
SELECT
  encode(dh.raw, 'hex'),
  dh.view,
  va.url,
  encode(va.data_hash, 'hex'),
  dr_deposit.deposit,
  DRepDistr.amount,
(DRepActivity.epoch_no - Max(coalesce(block.epoch_no, block_first_register.epoch_no))) <= DRepActivity.drep_activity AS active,
  second_to_newest_drep_registration.voting_anchor_id IS NOT NULL AS has_voting_anchor,
  encode(dr_voting_anchor.tx_hash, 'hex') AS tx_hash,
  newestRegister.time AS last_register_time
FROM
  drep_hash dh
  JOIN (
    SELECT
      dr.id,
      dr.drep_hash_id,
      dr.deposit,
      ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn
    FROM
      drep_registration dr
    WHERE
      dr.deposit IS NOT NULL) AS dr_deposit ON dr_deposit.drep_hash_id = dh.id
  AND dr_deposit.rn = 1
  LEFT JOIN (
    SELECT
      dr.id,
      dr.drep_hash_id,
      dr.voting_anchor_id,
      ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn,
      tx.hash AS tx_hash
    FROM
      drep_registration dr
      JOIN tx ON tx.id = dr.tx_id) AS dr_voting_anchor ON dr_voting_anchor.drep_hash_id = dh.id
    AND dr_voting_anchor.rn = 1
  LEFT JOIN (
    SELECT
      dr.id,
      dr.drep_hash_id,
      dr.voting_anchor_id,
      ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn
    FROM
      drep_registration dr) AS second_to_newest_drep_registration ON second_to_newest_drep_registration.drep_hash_id = dh.id
    AND second_to_newest_drep_registration.rn = 2
  LEFT JOIN DRepDistr ON DRepDistr.hash_id = dh.id
    AND DRepDistr.rn = 1
  LEFT JOIN voting_anchor va ON va.id = dr_voting_anchor.voting_anchor_id
  CROSS JOIN DRepActivity
  LEFT JOIN voting_procedure AS voting_procedure ON voting_procedure.drep_voter = dh.id
  LEFT JOIN tx AS tx ON tx.id = voting_procedure.tx_id
  LEFT JOIN block AS block ON block.id = tx.block_id
  LEFT JOIN (
    SELECT
      block.time,
      dr.drep_hash_id,
      ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn
    FROM
      drep_registration dr
      JOIN tx ON tx.id = dr.tx_id
      JOIN block ON block.id = tx.block_id
    WHERE
      NOT (dr.deposit < 0)) AS newestRegister ON newestRegister.drep_hash_id = dh.id
  AND newestRegister.rn = 1
  LEFT JOIN (
    SELECT
      dr.tx_id,
      dr.drep_hash_id,
      ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id ASC) AS rn
    FROM
      drep_registration dr) AS dr_first_register ON dr_first_register.drep_hash_id = dh.id
    AND dr_first_register.rn = 1
  LEFT JOIN tx AS tx_first_register ON tx_first_register.id = dr_first_register.tx_id
  LEFT JOIN block AS block_first_register ON block_first_register.id = tx_first_register.block_id
GROUP BY
  dh.raw,
  second_to_newest_drep_registration.voting_anchor_id,
  dh.view,
  va.url,
  va.data_hash,
  dr_deposit.deposit,
  DRepDistr.amount,
  DRepActivity.epoch_no,
  DRepActivity.drep_activity,
  dr_voting_anchor.tx_hash,
  newestRegister.time
