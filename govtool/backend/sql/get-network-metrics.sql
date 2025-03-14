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
CommitteeMembers AS (
    SELECT DISTINCT ON (cm.committee_hash_id)
        cr.id,
        block.time,
        encode(cold_key_hash.raw, 'hex') cold_key,
        encode(hot_key_hash.raw, 'hex') hot_key
    FROM committee_registration cr
    JOIN tx ON tx.id = cr.tx_id
    JOIN block ON block.id = tx.block_id
    JOIN committee_hash cold_key_hash ON cr.cold_key_id = cold_key_hash.id
    JOIN committee_hash hot_key_hash ON cr.hot_key_id = hot_key_hash.id
    JOIN committee_member cm ON cm.committee_hash_id = cold_key_hash.id OR cm.committee_hash_id = hot_key_hash.id
    LEFT JOIN committee_de_registration cdr ON cdr.cold_key_id = cold_key_hash.id
    CROSS JOIN CurrentEpoch
    WHERE
        cdr.id IS NULL AND cm.expiration_epoch > CurrentEpoch.no 
),
NoOfCommitteeMembers AS (
	SELECT COUNT(*) total FROM CommitteeMembers
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
TotalDRepDistr AS (
	SELECT SUM(COALESCE(amount, 0))::bigint total_drep_distr FROM drep_distr where epoch_no = (SELECT no from CurrentEpoch)
),
LatestGovAction AS (
    SELECT gap.id, gap.enacted_epoch
    FROM gov_action_proposal gap
    JOIN CurrentEpoch ce ON gap.enacted_epoch < ce.no
    ORDER BY gap.id DESC
    LIMIT 1
),
CommitteeThreshold AS (
    SELECT
        c.*
    FROM committee c
    LEFT JOIN LatestGovAction lga ON c.gov_action_proposal_id = lga.id
    WHERE (c.gov_action_proposal_id IS NOT NULL AND lga.id IS NOT NULL)
        OR (c.gov_action_proposal_id IS NULL)
)
SELECT
    UniqueDelegators.count AS unique_delegators,
    TotalDelegations.count AS total_delegations,
    TotalGovActionProposals.count AS total_gov_action_proposals,
    TotalDRepVotes.count AS total_drep_votes,
    TotalRegisteredDReps.unique_registrations AS total_registered_dreps,
	TotalDRepDistr.total_drep_distr,
    TotalActiveDReps.unique_active_drep_registrations AS total_active_dreps,
    TotalInactiveDReps.total_inactive_dreps AS total_inactive_dreps,
    TotalActiveCIP119CompliantDReps.unique_active_cip119_compliant_drep_registrations AS total_active_cip119_compliant_dreps,
    TotalRegisteredDirectVoters.unique_direct_voters AS total_registered_direct_voters,
    NoOfCommitteeMembers.total no_of_committee_members,
    CommitteeThreshold.quorum_numerator,
    CommitteeThreshold.quorum_denominator
FROM UniqueDelegators
CROSS JOIN TotalDRepDistr
CROSS JOIN TotalDelegations
CROSS JOIN TotalGovActionProposals
CROSS JOIN TotalDRepVotes
CROSS JOIN TotalRegisteredDReps
CROSS JOIN TotalActiveDReps
CROSS JOIN TotalInactiveDReps
CROSS JOIN TotalActiveCIP119CompliantDReps
CROSS JOIN TotalRegisteredDirectVoters
CROSS JOIN NoOfCommitteeMembers
CROSS JOIN CommitteeThreshold
