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
ActiveDRepBoundaryEpoch AS (
    SELECT epoch_no - drep_activity AS epoch_no FROM DRepActivity
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
TotalRegisteredDReps AS (
    SELECT COUNT(DISTINCT dh.raw) AS unique_registrations
    FROM drep_registration dr
    JOIN drep_hash dh ON dr.drep_hash_id = dh.id
    WHERE dr.deposit > 0
),
TotalActiveDReps AS (
    SELECT COUNT(DISTINCT RankedDRepRegistration.drep_hash_id) AS unique_active_drep_registrations
    FROM RankedDRepRegistration
    LEFT JOIN LatestVoteEpoch lve ON lve.drep_id = RankedDRepRegistration.drep_hash_id
    WHERE 
        (RankedDRepRegistration.epoch_no >= (SELECT epoch_no FROM ActiveDRepBoundaryEpoch))
        OR (lve.epoch_no >= (SELECT epoch_no FROM ActiveDRepBoundaryEpoch))
),
TotalInactiveDReps AS (
    SELECT
        TotalRegisteredDReps.unique_registrations - TotalActiveDReps.unique_active_drep_registrations AS total_inactive_dreps
    FROM TotalRegisteredDReps
    CROSS JOIN TotalActiveDReps
),
TotalActiveCIP119CompliantDReps AS (
    SELECT COUNT(DISTINCT RankedDRepRegistration.drep_hash_id) AS unique_active_cip119_compliant_drep_registrations
    FROM RankedDRepRegistration
    JOIN voting_anchor va ON va.id = RankedDRepRegistration.voting_anchor_id
    JOIN off_chain_vote_data ocvd ON ocvd.voting_anchor_id = va.id
    JOIN off_chain_vote_drep_data ocvdd ON ocvdd.off_chain_vote_data_id = ocvd.id
    WHERE ocvdd.given_name IS NOT NULL
    AND (RankedDRepRegistration.epoch_no >= (SELECT epoch_no FROM ActiveDRepBoundaryEpoch)
    OR (EXISTS (
        SELECT 1 FROM LatestVoteEpoch lve
        WHERE lve.drep_id = RankedDRepRegistration.drep_hash_id
        AND lve.epoch_no >= (SELECT epoch_no FROM ActiveDRepBoundaryEpoch)
    )))
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
CurrentBlock AS (
    SELECT MAX(block_no) AS block_no FROM block
),
UniqueDelegators AS (
    SELECT COUNT(DISTINCT addr_id) AS count FROM delegation_vote
),
TotalDelegations AS (
    SELECT COUNT(*) AS count FROM delegation_vote
),
TotalGovActionProposals AS (
    SELECT COUNT(DISTINCT (tx_id, INDEX)) AS count FROM gov_action_proposal
),
TotalDRepVotes AS (
    SELECT COUNT(*) AS count FROM voting_procedure WHERE voter_role = 'DRep'
),
TotalStakeControlledBySPOs AS (
    SELECT SUM(ps.stake)::bigint AS total FROM pool_stat ps WHERE ps.epoch_no = (SELECT no FROM CurrentEpoch)
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
TotalRegisteredDirectVoters AS (
    SELECT COUNT(DISTINCT rdr.drep_hash_id) AS unique_direct_voters
    FROM RankedDRepRegistration rdr
    LEFT JOIN LatestExistingVotingAnchor leva ON leva.drep_hash_id = rdr.drep_hash_id
    LEFT JOIN HasNonDeregisterVotingAnchor hndva ON hndva.drep_hash_id = rdr.drep_hash_id
    WHERE rdr.rn = 1 AND COALESCE(rdr.deposit, 0) >= 0 AND (leva.url IS NULL OR hndva.value = true)
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
),
TotalDRepDistr AS (
	SELECT SUM(COALESCE(amount, 0))::bigint total_drep_distr FROM drep_distr where epoch_no = (SELECT no from CurrentEpoch)
)
SELECT
    CurrentEpoch.no AS epoch_no,
    CurrentBlock.block_no,
    UniqueDelegators.count AS unique_delegators,
    TotalDelegations.count AS total_delegations,
    TotalGovActionProposals.count AS total_gov_action_proposals,
    TotalDRepVotes.count AS total_drep_votes,
    TotalRegisteredDReps.unique_registrations AS total_registered_dreps,
	TotalDRepDistr.total_drep_distr,
    COALESCE(TotalStakeControlledByActiveDReps.total, 0) + COALESCE(AlwaysAbstainVotingPower.amount, 0) + COALESCE(AlwaysNoConfidenceVotingPower.amount, 0) AS total_stake_controlled_dreps,
    COALESCE(TotalStakeControlledBySPOs.total, 0) AS total_stake_controlled_by_spos,
    TotalActiveDReps.unique_active_drep_registrations AS total_active_dreps,
    TotalInactiveDReps.total_inactive_dreps AS total_inactive_dreps,
    TotalActiveCIP119CompliantDReps.unique_active_cip119_compliant_drep_registrations AS total_active_cip119_compliant_dreps,
    TotalRegisteredDirectVoters.unique_direct_voters AS total_registered_direct_voters,
    AlwaysAbstainVotingPower.amount AS always_abstain_voting_power,
    AlwaysNoConfidenceVotingPower.amount AS always_no_confidence_voting_power,
    meta.network_name
FROM CurrentEpoch
CROSS JOIN CurrentBlock
CROSS JOIN UniqueDelegators
CROSS JOIN TotalDRepDistr
CROSS JOIN TotalDelegations
CROSS JOIN TotalGovActionProposals
CROSS JOIN TotalDRepVotes
CROSS JOIN TotalRegisteredDReps
CROSS JOIN TotalStakeControlledByActiveDReps
CROSS JOIN TotalStakeControlledBySPOs
CROSS JOIN TotalActiveDReps
CROSS JOIN TotalInactiveDReps
CROSS JOIN TotalActiveCIP119CompliantDReps
CROSS JOIN TotalRegisteredDirectVoters
CROSS JOIN AlwaysAbstainVotingPower
CROSS JOIN AlwaysNoConfidenceVotingPower
CROSS JOIN meta;
