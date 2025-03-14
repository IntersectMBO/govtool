WITH DRepActivity AS (
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
),
LatestVotingProcedure AS (
    SELECT
        vp.*,
        ROW_NUMBER() OVER (PARTITION BY drep_voter ORDER BY tx_id DESC) AS rn
    FROM
        voting_procedure vp
),
DRepDistr AS (
    SELECT
        drep_distr.*,
        ROW_NUMBER() OVER (PARTITION BY drep_hash.id ORDER BY drep_distr.epoch_no DESC) AS rn
    FROM
        drep_distr
        JOIN drep_hash ON drep_hash.id = drep_distr.hash_id
),
CurrentEpoch AS (
    SELECT MAX(no) AS no FROM epoch
),
LatestVoteEpoch AS (
    SELECT
        block.epoch_no,
        lvp.drep_voter AS drep_id
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
        encode(tx.hash, 'hex') AS tx_hash,
        block.epoch_no
    FROM
        drep_registration dr
    JOIN tx ON tx.id = dr.tx_id
    JOIN block ON block.id = tx.block_id
),
TotalStakeControlledByActiveDReps AS (
    SELECT
        COALESCE(SUM(dd.amount), 0)::bigint AS total
    FROM
        drep_hash dh
    LEFT JOIN DRepDistr dd ON dd.hash_id = dh.id AND dd.rn = 1
    LEFT JOIN RankedDRepRegistration rd ON dd.hash_id = rd.drep_hash_id AND rd.rn = 1
    LEFT JOIN LatestVoteEpoch lve ON lve.drep_id = dh.id
    CROSS JOIN DRepActivity
    WHERE
        dd.epoch_no = (SELECT no FROM CurrentEpoch)
        AND COALESCE(rd.deposit, 0) >= 0
        AND ((DRepActivity.epoch_no - GREATEST(COALESCE(lve.epoch_no, 0), COALESCE(rd.epoch_no, 0))) <= DRepActivity.drep_activity)
),
TotalStakeControlledBySPOs AS (
    SELECT SUM(ps.stake)::bigint AS total FROM pool_stat ps WHERE ps.epoch_no = (SELECT no FROM CurrentEpoch)
),
AlwaysAbstainVotingPower AS (
    SELECT COALESCE((SELECT amount FROM drep_hash
        LEFT JOIN drep_distr ON drep_hash.id = drep_distr.hash_id
        WHERE drep_hash.view = 'drep_always_abstain'
        ORDER BY epoch_no DESC LIMIT 1), 0) AS amount
),
AlwaysNoConfidenceVotingPower AS (
    SELECT COALESCE((SELECT amount FROM drep_hash
        LEFT JOIN drep_distr ON drep_hash.id = drep_distr.hash_id
        WHERE drep_hash.view = 'drep_always_no_confidence'
        ORDER BY epoch_no DESC LIMIT 1), 0) AS amount
)
SELECT
  COALESCE(TotalStakeControlledByActiveDReps.total, 0) + COALESCE(AlwaysNoConfidenceVotingPower.amount, 0) AS total_stake_controlled_by_active_dreps,
  COALESCE(TotalStakeControlledBySPOs.total, 0) AS total_stake_controlled_by_spos,
  AlwaysAbstainVotingPower.amount AS always_abstain_voting_power,
  AlwaysNoConfidenceVotingPower.amount AS always_no_confidence_voting_power
FROM TotalStakeControlledByActiveDReps
LEFT JOIN TotalStakeControlledBySPOs ON TRUE
LEFT JOIN AlwaysAbstainVotingPower ON TRUE
LEFT JOIN AlwaysNoConfidenceVotingPower ON TRUE