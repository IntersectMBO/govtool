WITH LatestDrepDistr AS (
    SELECT
        *,
        ROW_NUMBER() OVER (PARTITION BY hash_id ORDER BY epoch_no DESC) AS rn
    FROM
        drep_distr
),
EpochUtils AS (
    SELECT
        (Max(end_time) - Min(end_time)) /(Max(NO) - Min(NO)) AS epoch_duration,
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
    gov_action_proposal.id,
    encode(creator_tx.hash, 'hex'),
    gov_action_proposal.index,
    gov_action_proposal.type::text,
    gov_action_proposal.description::json,
    epoch_utils.last_epoch_end_time + epoch_utils.epoch_duration *(gov_action_proposal.expiration - epoch_utils.last_epoch_no),
    creator_block.time,
    /* created date */
    voting_anchor.url,
    encode(voting_anchor.data_hash, 'hex'),
    coalesce(Sum(ldd.amount) FILTER (WHERE voting_procedure.vote::text = 'Yes'), 0) +(
        CASE WHEN gov_action_proposal.type = 'NoConfidence' THEN
            always_no_confidence_voting_power.amount
        ELSE
            0
        END) "yes_votes",
    coalesce(Sum(ldd.amount) FILTER (WHERE voting_procedure.vote::text = 'No'), 0) +(
        CASE WHEN gov_action_proposal.type = 'NoConfidence' THEN
            0
        ELSE
            always_no_confidence_voting_power.amount
        END) "no_votes",
    coalesce(Sum(ldd.amount) FILTER (WHERE voting_procedure.vote::text = 'Abstain'), 0) + always_abstain_voting_power.amount "abstain_votes"
FROM
    gov_action_proposal
    CROSS JOIN EpochUtils AS epoch_utils
    CROSS JOIN always_no_confidence_voting_power
    CROSS JOIN always_abstain_voting_power
    JOIN tx AS creator_tx ON creator_tx.id = gov_action_proposal.tx_id
    JOIN block AS creator_block ON creator_block.id = creator_tx.block_id
    JOIN voting_anchor ON voting_anchor.id = gov_action_proposal.voting_anchor_id
    LEFT JOIN voting_procedure ON voting_procedure.gov_action_proposal_id = gov_action_proposal.id
    LEFT JOIN LatestDrepDistr ldd ON ldd.hash_id = voting_procedure.drep_voter
        AND ldd.rn = 1
WHERE (NOT ?
    OR (concat(encode(creator_tx.hash, 'hex'), '#', gov_action_proposal.index) IN ?))
AND gov_action_proposal.expiration >(
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
