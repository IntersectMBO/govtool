WITH LatestDrepDistr AS (
    SELECT
        *,
        ROW_NUMBER() OVER (PARTITION BY hash_id ORDER BY epoch_no DESC) AS rn
    FROM
        drep_distr
),
EpochUtils AS (
    SELECT
        (Max(end_time) - Min(end_time)) / (Max(NO) - Min(NO)) AS epoch_duration,
        Max(NO) AS last_epoch_no,
        Max(end_time) AS last_epoch_end_time
    FROM
        epoch
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
)
SELECT
    gov_action_proposal.id AS proposal_id,
    encode(creator_tx.hash, 'hex') AS tx_hash,
    gov_action_proposal.index,
    gov_action_proposal.type::text,
    CASE 
        WHEN gov_action_proposal.type = 'TreasuryWithdrawals' THEN
            json_build_object('Reward Address', stake_address.view, 'Amount', treasury_withdrawal.amount)
        WHEN gov_action_proposal.type::text = 'InfoAction' THEN
            json_build_object()
        ELSE
            NULL
    END AS description,
    epoch_utils.last_epoch_end_time + epoch_utils.epoch_duration * (gov_action_proposal.expiration - epoch_utils.last_epoch_no) AS expiry_date,
    gov_action_proposal.expiration AS expiry_epoch_no,
    creator_block.time AS created_date,
    creator_block.epoch_no AS created_epoch_no,
    voting_anchor.url,
    encode(voting_anchor.data_hash, 'hex') AS metadata_hash,
    off_chain_vote_gov_action_data.title,
    off_chain_vote_gov_action_data.abstract,
    off_chain_vote_gov_action_data.motivation,
    off_chain_vote_gov_action_data.rationale,
    coalesce(Sum(ldd.amount) FILTER (WHERE voting_procedure.vote::text = 'Yes'), 0) +
        CASE 
            WHEN gov_action_proposal.type = 'NoConfidence' THEN always_no_confidence_voting_power.amount
            ELSE 0
        END AS yes_votes,
    coalesce(Sum(ldd.amount) FILTER (WHERE voting_procedure.vote::text = 'No'), 0) +
        CASE 
            WHEN gov_action_proposal.type = 'NoConfidence' THEN 0
            ELSE always_no_confidence_voting_power.amount
        END AS no_votes,
    coalesce(Sum(ldd.amount) FILTER (WHERE voting_procedure.vote::text = 'Abstain'), 0) + always_abstain_voting_power.amount AS abstain_votes
FROM
    gov_action_proposal
    LEFT JOIN treasury_withdrawal ON gov_action_proposal.id = treasury_withdrawal.gov_action_proposal_id
    LEFT JOIN stake_address ON stake_address.id = treasury_withdrawal.stake_address_id
    CROSS JOIN EpochUtils AS epoch_utils
    CROSS JOIN always_no_confidence_voting_power
    CROSS JOIN always_abstain_voting_power
    CROSS JOIN off_chain_vote_gov_action_data ON off_chain_vote_gov_action_data.off_chain_vote_data_id = off_chain_vote_data.id
    JOIN tx AS creator_tx ON creator_tx.id = gov_action_proposal.tx_id
    JOIN block AS creator_block ON creator_block.id = creator_tx.block_id
    LEFT JOIN voting_anchor ON voting_anchor.id = gov_action_proposal.voting_anchor_id
    LEFT JOIN off_chain_vote_data ON off_chain_vote_data.voting_anchor_id = voting_anchor.id
    LEFT JOIN voting_procedure ON voting_procedure.gov_action_proposal_id = gov_action_proposal.id
    LEFT JOIN LatestDrepDistr ldd ON ldd.hash_id = voting_procedure.drep_voter AND ldd.rn = 1
WHERE (NOT ?
    OR (concat(encode(creator_tx.hash, 'hex'), '#', gov_action_proposal.index) IN ?))
AND gov_action_proposal.expiration > (
    SELECT 
        Max(NO)
    FROM 
        epoch)
    AND gov_action_proposal.ratified_epoch IS NULL
    AND gov_action_proposal.enacted_epoch IS NULL
    AND gov_action_proposal.expired_epoch IS NULL
    AND gov_action_proposal.dropped_epoch IS NULL
GROUP BY
    (gov_action_proposal.id,
        stake_address.view,
        treasury_withdrawal.amount,
        creator_block.epoch_no,
        off_chain_vote_gov_action_data.title,
        off_chain_vote_gov_action_data.abstract,
        off_chain_vote_gov_action_data.motivation,
        off_chain_vote_gov_action_data.rationale,
        gov_action_proposal.index,
        creator_tx.hash,
        creator_block.time,
        epoch_utils.epoch_duration,
        epoch_utils.last_epoch_no,
        epoch_utils.last_epoch_end_time,
        voting_anchor.url,
        voting_anchor.data_hash,
        always_no_confidence_voting_power.amount,
        always_abstain_voting_power.amount)
