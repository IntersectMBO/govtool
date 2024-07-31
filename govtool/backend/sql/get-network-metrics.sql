WITH current_epoch AS (
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
total_registered_dreps AS (
    SELECT
        count(*) AS count
    FROM
        drep_hash
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
    current_epoch.no,
    current_block.block_no,
    unique_delegators.count,
    total_delegations.count,
    total_gov_action_proposals.count,
    total_drep_votes.count,
    total_registered_dreps.count,
    always_abstain_voting_power.amount,
    always_no_confidence_voting_power.amount,
    network_name
FROM
    current_epoch
    CROSS JOIN current_block
    CROSS JOIN unique_delegators
    CROSS JOIN total_delegations
    CROSS JOIN total_gov_action_proposals
    CROSS JOIN total_drep_votes
    CROSS JOIN total_registered_dreps
    CROSS JOIN always_abstain_voting_power
    CROSS JOIN always_no_confidence_voting_power
    CROSS JOIN meta;
