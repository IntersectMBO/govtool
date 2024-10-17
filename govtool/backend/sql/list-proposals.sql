WITH LatestDrepDistr AS (
    SELECT
        *,
        ROW_NUMBER() OVER (PARTITION BY hash_id ORDER BY epoch_no DESC) AS rn
    FROM
        drep_distr
),
LatestEpoch AS (
    SELECT
        start_time,
        no
    FROM
        epoch ORDER BY no DESC LIMIT 1
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
    (
        case when gov_action_proposal.type = 'TreasuryWithdrawals' then
            json_build_object('Reward Address', stake_address.view, 'Amount', treasury_withdrawal.amount)

            when gov_action_proposal.type::text = 'InfoAction' then
            json_build_object()

            when gov_action_proposal.type::text = 'HardForkInitiation' then
            json_build_object(
                'major', (gov_action_proposal.description->'contents'->1->>'major')::int, 
                'minor', (gov_action_proposal.description->'contents'->1->>'minor')::int
            )
        else
            null
        end
    ) as description,
    CASE
        WHEN meta.network_name::text = 'mainnet' THEN
            latest_epoch.start_time + (gov_action_proposal.expiration - latest_epoch.no)::bigint * INTERVAL '5 days' 
        ELSE
            latest_epoch.start_time + (gov_action_proposal.expiration - latest_epoch.no)::bigint * INTERVAL '1 day' 
    END AS expiry_date,
    gov_action_proposal.expiration,
    creator_block.time,
    creator_block.epoch_no,
    voting_anchor.url,
    encode(voting_anchor.data_hash, 'hex'),
    ROW_TO_JSON(proposal_params),
    off_chain_vote_gov_action_data.title,
    off_chain_vote_gov_action_data.abstract,
    off_chain_vote_gov_action_data.motivation,
    off_chain_vote_gov_action_data.rationale,
    coalesce(Sum(ldd_drep.amount) FILTER (WHERE voting_procedure.vote::text = 'Yes'), 0) +(
        CASE WHEN gov_action_proposal.type = 'NoConfidence' THEN
            always_no_confidence_voting_power.amount
        ELSE
            0
        END) "yes_votes",
    coalesce(Sum(ldd_drep.amount) FILTER (WHERE voting_procedure.vote::text = 'No'), 0) +(
        CASE WHEN gov_action_proposal.type = 'NoConfidence' THEN
            0
        ELSE
            always_no_confidence_voting_power.amount
        END) "no_votes",
    coalesce(Sum(ldd_drep.amount) FILTER (WHERE voting_procedure.vote::text = 'Abstain'), 0) + always_abstain_voting_power.amount "abstain_votes",
    coalesce(Sum(ldd_pool.amount) FILTER (WHERE voting_procedure.vote::text = 'Yes'), 0),
    coalesce(Sum(ldd_pool.amount) FILTER (WHERE voting_procedure.vote::text = 'No'), 0),
    coalesce(Sum(ldd_pool.amount) FILTER (WHERE voting_procedure.vote::text = 'Abstain'), 0),
    coalesce(vp_by_cc.ccYesVotes, 0),
    coalesce(vp_by_cc.ccNoVotes, 0),
    coalesce(vp_by_cc.ccAbstainVotes, 0),
    prev_gov_action.index as prev_gov_action_index,
    encode(prev_gov_action_tx.hash, 'hex') as prev_gov_action_tx_hash
FROM
    gov_action_proposal
    LEFT JOIN treasury_withdrawal
    on gov_action_proposal.id = treasury_withdrawal.gov_action_proposal_id
    LEFT JOIN stake_address
    on stake_address.id = treasury_withdrawal.stake_address_id
    CROSS JOIN LatestEpoch AS latest_epoch
    CROSS JOIN always_no_confidence_voting_power
    CROSS JOIN always_abstain_voting_power
    CROSS JOIN meta
    JOIN tx AS creator_tx ON creator_tx.id = gov_action_proposal.tx_id
    JOIN block AS creator_block ON creator_block.id = creator_tx.block_id
    LEFT JOIN voting_anchor ON voting_anchor.id = gov_action_proposal.voting_anchor_id
    LEFT JOIN param_proposal as proposal_params ON gov_action_proposal.param_proposal = proposal_params.id
    LEFT JOIN off_chain_vote_data ON off_chain_vote_data.voting_anchor_id = voting_anchor.id
    LEFT JOIN off_chain_vote_gov_action_data ON off_chain_vote_gov_action_data.off_chain_vote_data_id = off_chain_vote_data.id
    LEFT JOIN voting_procedure ON voting_procedure.gov_action_proposal_id = gov_action_proposal.id
    LEFT JOIN LatestDrepDistr ldd_drep ON ldd_drep.hash_id = voting_procedure.drep_voter
        AND ldd_drep.rn = 1
    LEFT JOIN LatestDrepDistr ldd_pool ON ldd_pool.hash_id = voting_procedure.pool_voter
        AND ldd_pool.rn = 1
    LEFT JOIN 
    (
        SELECT 
            gov_action_proposal_id,
            SUM(CASE WHEN vote = 'Yes' THEN 1 ELSE 0 END) AS ccYesVotes,
            SUM(CASE WHEN vote = 'No' THEN 1 ELSE 0 END) AS ccNoVotes,
            SUM(CASE WHEN vote = 'Abstain' THEN 1 ELSE 0 END) AS ccAbstainVotes
        FROM 
            voting_procedure
        WHERE 
            committee_voter IS NOT NULL
        GROUP BY 
            gov_action_proposal_id
    ) vp_by_cc
    ON gov_action_proposal.id = vp_by_cc.gov_action_proposal_id
    
    LEFT JOIN LatestDrepDistr ldd_cc ON ldd_cc.hash_id = voting_procedure.committee_voter
        AND ldd_cc.rn = 1
    LEFT JOIN gov_action_proposal AS prev_gov_action ON gov_action_proposal.prev_gov_action_proposal = prev_gov_action.id
    LEFT JOIN tx AS prev_gov_action_tx ON prev_gov_action.tx_id = prev_gov_action_tx.id
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
        stake_address.view,
        treasury_withdrawal.amount,
        creator_block.epoch_no,
        off_chain_vote_gov_action_data.title,
        off_chain_vote_gov_action_data.abstract,
        off_chain_vote_gov_action_data.motivation,
        off_chain_vote_gov_action_data.rationale,
        vp_by_cc.ccYesVotes,
        vp_by_cc.ccNoVotes,
        vp_by_cc.ccAbstainVotes,
        gov_action_proposal.index,
        creator_tx.hash,
        creator_block.time,
        latest_epoch.start_time,
        latest_epoch.no,
        proposal_params,
        voting_anchor.url,
        voting_anchor.data_hash,
        always_no_confidence_voting_power.amount,
        always_abstain_voting_power.amount,
        prev_gov_action.index,
        prev_gov_action_tx.hash,
        meta.network_name)