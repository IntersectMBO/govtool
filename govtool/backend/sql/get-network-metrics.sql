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
active_drep_boundary_epoch AS (
    SELECT 
        epoch_no - drep_activity AS epoch_no
    FROM
        DRepActivity
),
RankedDRep AS (
    SELECT
        dh.raw AS drep_hash_raw,
        b.epoch_no,
        dr.deposit,
        dr.voting_anchor_id,
        ROW_NUMBER() OVER (PARTITION BY dh.raw ORDER BY dr.tx_id DESC) AS rank
    FROM
        drep_hash dh
    JOIN
        drep_registration dr ON dh.id = dr.drep_hash_id
    JOIN
        tx t ON dr.tx_id = t.id
    JOIN
        block b ON t.block_id = b.id
    WHERE
        dr.deposit >= 0
    GROUP BY
        dh.raw,
        b.epoch_no,
        dr.voting_anchor_id,
        dr.deposit,
        dr.tx_id
),
current_epoch AS (
    SELECT
        Max(NO) AS no
    FROM
        epoch
),
current_block AS (
    SELECT
        Max(block_no) AS block_no
    FROM
        block
),
unique_delegators AS (
    SELECT
        count(DISTINCT (addr_id)) AS count
    FROM
        delegation_vote
),
total_delegations AS (
    SELECT
        count(*) AS count
    FROM
        delegation_vote
),
total_gov_action_proposals AS (
    SELECT
        count(DISTINCT (tx_id, INDEX)) AS count
    FROM
        gov_action_proposal
),
total_drep_votes AS (
    SELECT
        count(*) AS count
    FROM
        voting_procedure
    WHERE
        voter_role = 'DRep'
),
total_stake_controlled_by_dreps AS (
    SELECT
        SUM(dd.amount)::bigint AS total
    FROM
        drep_distr dd
    WHERE
        dd.epoch_no = (SELECT no FROM current_epoch)
),
total_stake_controlled_by_spos AS (
    SELECT
        SUM(ps.stake)::bigint AS total
    FROM
        pool_stat ps
    WHERE
        ps.epoch_no = (SELECT no FROM current_epoch)
),
total_registered_direct_voters AS (
    SELECT 
        COUNT(DISTINCT dh.raw) AS unique_direct_voters
    FROM 
        drep_registration dr
    JOIN
        drep_hash dh
    ON
        dr.drep_hash_id = dh.id
    LEFT JOIN 
        voting_anchor va
    ON 
        dr.voting_anchor_id = va.id
    WHERE 
        dr.deposit > 0 
        AND va.url IS NULL
),
total_registered_dreps AS (
    SELECT 
        count(DISTINCT dh.raw) AS unique_registrations
    FROM 
        drep_registration dr
    JOIN
        drep_hash dh
    ON
        dr.drep_hash_id = dh.id
    WHERE 
        dr.deposit > 0
),
total_active_dreps AS (
    SELECT
        count(DISTINCT drep_hash_raw) AS unique_active_drep_registrations
    FROM
        RankedDRep
    WHERE
        epoch_no >= (SELECT epoch_no FROM active_drep_boundary_epoch)
        AND rank = 1
),
total_inactive_dreps AS (
    SELECT
        total_registered_dreps.unique_registrations - total_active_dreps.unique_active_drep_registrations AS total_inactive_dreps
    FROM
        total_registered_dreps
    CROSS JOIN
        total_active_dreps
),
total_active_cip119_compliant_dreps AS (
    SELECT
        count(DISTINCT drep_hash_raw) AS unique_active_cip119_compliant_drep_registrations
    FROM
        RankedDRep
    JOIN
        voting_anchor va on va.id = RankedDRep.voting_anchor_id
    JOIN off_chain_vote_data ocvd on ocvd.voting_anchor_id = va.id
    JOIN off_chain_vote_drep_data ocvdd on ocvdd.off_chain_vote_data_id = ocvd.id
    WHERE
    -- given_name is the only compulsory field in CIP-119
        ocvdd.given_name IS NOT NULL
    AND
        epoch_no >= (SELECT epoch_no FROM active_drep_boundary_epoch)
    AND
        rank = 1
),
always_abstain_voting_power AS (
    SELECT
        coalesce((
            SELECT
                amount
            FROM drep_hash
        LEFT JOIN drep_distr ON drep_hash.id = drep_distr.hash_id
        WHERE
            drep_hash.view = 'drep_always_abstain' ORDER BY epoch_no DESC LIMIT 1), 0) AS amount
),
always_no_confidence_voting_power AS (
    SELECT
        coalesce((
            SELECT
                amount
            FROM drep_hash
        LEFT JOIN drep_distr ON drep_hash.id = drep_distr.hash_id
    WHERE
        drep_hash.view = 'drep_always_no_confidence' ORDER BY epoch_no DESC LIMIT 1), 0) AS amount
)
SELECT
    current_epoch.no as epoch_no,
    current_block.block_no,
    unique_delegators.count as unique_delegators,
    total_delegations.count as total_delegations,
    total_gov_action_proposals.count as total_gov_action_proposals,
    total_drep_votes.count as total_drep_votes,
    total_registered_dreps.unique_registrations as total_registered_dreps,
    COALESCE(total_stake_controlled_by_dreps.total, 0) as total_stake_controlled_by_dreps,
    COALESCE(total_stake_controlled_by_spos.total, 0) as total_stake_controlled_by_spos,
    total_active_dreps.unique_active_drep_registrations as total_active_dreps,
    total_inactive_dreps.total_inactive_dreps as total_inactive_dreps,
    total_active_cip119_compliant_dreps.unique_active_cip119_compliant_drep_registrations as total_active_cip119_compliant_dreps,
    total_registered_direct_voters.unique_direct_voters as total_registered_direct_voters,
    always_abstain_voting_power.amount as always_abstain_voting_power,
    always_no_confidence_voting_power.amount as always_no_confidence_voting_power,
    network_name
FROM
    current_epoch
    CROSS JOIN current_block
    CROSS JOIN unique_delegators
    CROSS JOIN total_delegations
    CROSS JOIN total_gov_action_proposals
    CROSS JOIN total_drep_votes
    CROSS JOIN total_registered_dreps
    CROSS JOIN total_stake_controlled_by_dreps
    CROSS JOIN total_stake_controlled_by_spos
    CROSS JOIN total_active_dreps
    CROSS JOIN total_inactive_dreps
    CROSS JOIN total_active_cip119_compliant_dreps
    CROSS JOIN total_registered_direct_voters
    CROSS JOIN always_abstain_voting_power
    CROSS JOIN always_no_confidence_voting_power
    CROSS JOIN meta
GROUP BY
    current_epoch.no,
    current_block.block_no,
    unique_delegators.count,
    total_delegations.count,
    total_gov_action_proposals.count,
    total_drep_votes.count,
    total_registered_dreps.unique_registrations,
    total_stake_controlled_by_dreps.total,
    total_stake_controlled_by_spos.total,
    total_active_dreps.unique_active_drep_registrations,
    total_inactive_dreps.total_inactive_dreps,
    total_active_cip119_compliant_dreps.unique_active_cip119_compliant_drep_registrations,
    total_registered_direct_voters.unique_direct_voters,
    always_abstain_voting_power.amount,
    always_no_confidence_voting_power.amount,
    network_name;

