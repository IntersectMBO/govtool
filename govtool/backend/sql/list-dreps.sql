WITH DRepDistr AS (
  SELECT
    drep_distr.*,
    ROW_NUMBER() OVER (PARTITION BY drep_hash.id ORDER BY drep_distr.epoch_no DESC) AS rn
  FROM
    drep_distr
    JOIN drep_hash ON drep_hash.id = drep_distr.hash_id
),
DRepActivity AS (
  SELECT
    drep_activity,
    epoch_no
  FROM
    epoch_param
  WHERE
    epoch_no IS NOT NULL
  ORDER BY
    epoch_no DESC
  LIMIT 1
),
LatestVotingProcedure AS (
  SELECT
    vp.*,
    ROW_NUMBER() OVER (PARTITION BY drep_voter ORDER BY tx_id DESC) AS rn
  FROM
    voting_procedure vp
),
LatestVoteEpoch AS (
  SELECT
    block.epoch_no,
    lvp.drep_voter as drep_id
  FROM
    LatestVotingProcedure lvp
    JOIN tx ON tx.id = lvp.tx_id
    JOIN block ON block.id = tx.block_id
  WHERE
    lvp.rn = 1
),
RankedDRepRegistration AS (
	SELECT
      dr.id,
      dr.drep_hash_id,
      dr.deposit,
      dr.voting_anchor_id,
      ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn,
      encode(tx.hash, 'hex') AS tx_hash
    FROM
      drep_registration dr
  JOIN tx ON tx.id = dr.tx_id
),
FetchError AS (
  SELECT
    fetch_error as message,
    voting_anchor_id
  FROM
    off_chain_vote_fetch_error
  WHERE
    fetch_time = (
      SELECT
      max(fetch_time)
      FROM
      off_chain_vote_fetch_error
    )
  GROUP BY
    fetch_error,
    voting_anchor_id
),
LatestExistingVotingAnchor AS (
  SELECT
    subquery.drep_registration_id,
    subquery.drep_hash_id,
    subquery.voting_anchor_id,
    subquery.url,
    subquery.metadata_hash,
    subquery.ocvd_id
  FROM (
    SELECT
      dr.id AS drep_registration_id,
      dr.drep_hash_id,
      va.id AS voting_anchor_id,
      va.url,
      encode(va.data_hash, 'hex') AS metadata_hash,
      ocvd.id AS ocvd_id,
      ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn
    FROM
      drep_registration dr
    JOIN voting_anchor va ON dr.voting_anchor_id = va.id
    JOIN off_chain_vote_data ocvd ON va.id = ocvd.voting_anchor_id
    WHERE
      ocvd.voting_anchor_id IS NOT NULL
  ) subquery
  WHERE
    subquery.rn = 1
),
HasNonDeregisterVotingAnchor AS (
  SELECT
    dr.drep_hash_id,
    EXISTS (
      SELECT 1
      FROM drep_registration dr_sub
      WHERE 
        dr_sub.drep_hash_id = dr.drep_hash_id
        AND dr_sub.voting_anchor_id IS NULL
        AND COALESCE(dr_sub.deposit, 0) >= 0
    ) AS value
  FROM
    drep_registration dr
  GROUP BY
    dr.drep_hash_id
),
DRepData AS (
  SELECT DISTINCT ON (dh.raw)
    encode(dh.raw, 'hex') drep_hash,
    dh.view,
    dh.has_script,
    leva.url,
    leva.metadata_hash,
    COALESCE(dr_deposit.deposit, 0) as deposit,
    DRepDistr.amount,
    (DRepActivity.epoch_no - GREATEST(COALESCE(voting_procedure_block.epoch_no, block_first_register.epoch_no), lve.epoch_no, newestRegister.epoch_no)) <= DRepActivity.drep_activity AS active,
    RankedDRepRegistration.tx_hash,
    newestRegister.time AS last_register_time,
    COALESCE(RankedDRepRegistration.deposit, 0) as latest_deposit,
    hndva.value AS has_non_deregister_voting_anchor,
    fetch_error.message AS fetch_error,
    off_chain_vote_drep_data.payment_address,
    off_chain_vote_drep_data.given_name,
    off_chain_vote_drep_data.objectives,
    off_chain_vote_drep_data.motivations,
    off_chain_vote_drep_data.qualifications,
    off_chain_vote_drep_data.image_url,
    off_chain_vote_drep_data.image_hash
  FROM
    drep_hash dh
    JOIN RankedDRepRegistration ON RankedDRepRegistration.drep_hash_id = dh.id AND RankedDRepRegistration.rn = 1
    JOIN (
      SELECT
        dr.id,
        dr.drep_hash_id,
        dr.deposit,
        ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn
      FROM
        drep_registration dr
      WHERE
        dr.deposit IS NOT NULL
      ) AS dr_deposit ON dr_deposit.drep_hash_id = dh.id AND dr_deposit.rn = 1
    LEFT JOIN (
      SELECT
        dr.id,
        dr.drep_hash_id,
        dr.deposit,
        ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn
      FROM
        drep_registration dr
    ) AS latestDeposit ON latestDeposit.drep_hash_id = dh.id AND latestDeposit.rn = 1	
    LEFT JOIN LatestExistingVotingAnchor leva ON leva.drep_hash_id = dh.id
    LEFT JOIN DRepDistr ON DRepDistr.hash_id = dh.id AND DRepDistr.rn = 1
    LEFT JOIN FetchError fetch_error ON fetch_error.voting_anchor_id = leva.voting_anchor_id
    LEFT JOIN HasNonDeregisterVotingAnchor hndva ON hndva.drep_hash_id = dh.id
    LEFT JOIN off_chain_vote_data ocvd ON ocvd.voting_anchor_id = leva.voting_anchor_id
    LEFT JOIN off_chain_vote_drep_data ON off_chain_vote_drep_data.off_chain_vote_data_id = ocvd.id 
    LEFT JOIN voting_procedure ON voting_procedure.drep_voter = dh.id
    LEFT JOIN tx voting_procedure_transaction ON voting_procedure_transaction.id = voting_procedure.tx_id
    LEFT JOIN block voting_procedure_block ON voting_procedure_block.id = voting_procedure_transaction.block_id
    LEFT JOIN (
      SELECT
        block.epoch_no,
        block.time,
        dr.drep_hash_id,
        ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn
      FROM
      drep_registration dr
      JOIN tx ON tx.id = dr.tx_id
      JOIN block ON block.id = tx.block_id
      WHERE
        COALESCE(dr.deposit, 0) >= 0
      ) AS newestRegister ON newestRegister.drep_hash_id = dh.id AND newestRegister.rn = 1
      LEFT JOIN (
      SELECT
        dr.tx_id,
        dr.drep_hash_id,
        ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id ASC) AS rn
      FROM
        drep_registration dr
    ) AS dr_first_register ON dr_first_register.drep_hash_id = dh.id AND dr_first_register.rn = 1
    LEFT JOIN tx AS tx_first_register ON tx_first_register.id = dr_first_register.tx_id
    LEFT JOIN block AS block_first_register ON block_first_register.id = tx_first_register.block_id
    LEFT JOIN LatestVoteEpoch lve ON lve.drep_id = dh.id
    CROSS JOIN DRepActivity
  GROUP BY
    dh.raw,
    dh.view,
    dh.has_script,
    leva.url,
    leva.metadata_hash,
    dr_deposit.deposit,
    DRepDistr.amount,
    DRepActivity.epoch_no,
    voting_procedure_block.epoch_no,
    block_first_register.epoch_no,
    lve.epoch_no, newestRegister.epoch_no,
    DRepActivity.drep_activity,
    RankedDRepRegistration.tx_hash,
    newestRegister.time,
    RankedDRepRegistration.deposit,
    hndva.value,
    fetch_error.message,
    off_chain_vote_drep_data.payment_address,
    off_chain_vote_drep_data.given_name,
    off_chain_vote_drep_data.objectives,
    off_chain_vote_drep_data.motivations,
    off_chain_vote_drep_data.qualifications,
    off_chain_vote_drep_data.image_url,
    off_chain_vote_drep_data.image_hash
)
SELECT * FROM DRepData
WHERE
  (
    COALESCE(?, '') = '' OR
    (CASE WHEN LENGTH(?) % 2 = 0 AND ? ~ '^[0-9a-fA-F]+$' THEN drep_hash = ? ELSE false END) OR
    view ILIKE ? OR
    given_name ILIKE ? OR
    payment_address ILIKE ? OR
    objectives ILIKE ? OR
    motivations ILIKE ? OR
    qualifications ILIKE ?
  )