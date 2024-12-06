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
),
committee_data AS (
    SELECT DISTINCT ON (ch.raw)
        encode(ch.raw, 'hex') AS hash,
        cm.expiration_epoch,
        ch.has_script
    FROM
        committee_member cm
    JOIN 
        committee_hash ch ON cm.committee_hash_id = ch.id
    ORDER BY ch.raw, cm.expiration_epoch DESC
),
parsed_description AS (
    SELECT
        gov_action_proposal.id,
        description->'tag' AS tag,
        description->'contents'->1 AS members_to_be_removed,
        description->'contents'->2 AS members,
        description->'contents'->3 AS threshold
    FROM
        gov_action_proposal
    WHERE
        gov_action_proposal.type = 'NewCommittee'
),
members_to_be_removed AS (
    SELECT
        id,
        json_agg(value->>'keyHash') AS members_to_be_removed
    FROM
        parsed_description,
        json_array_elements(members_to_be_removed::json) AS value
    GROUP BY
        id
),
processed_current_members AS (
    SELECT
        pd.id,
        json_agg(
            json_build_object(
                'hash', regexp_replace(kv.key, '^keyHash-', ''),
                'newExpirationEpoch', kv.value::int
            )
        ) AS current_members
    FROM
        parsed_description pd,
        jsonb_each_text(pd.members) AS kv(key, value)
    GROUP BY
        pd.id
),
enriched_current_members AS (
    SELECT
        pcm.id,
        json_agg(
            json_build_object(
                'hash', cm.hash,
                'expirationEpoch', cm.expiration_epoch,
                'hasScript', cm.has_script,
                'newExpirationEpoch', (member->>'newExpirationEpoch')::int
            )
        ) AS enriched_members
    FROM
        processed_current_members pcm
    LEFT JOIN
        json_array_elements(pcm.current_members) AS member ON true
    LEFT JOIN
        committee_data cm ON cm.hash = encode(decode(member->>'hash', 'hex'), 'hex')
    GROUP BY
        pcm.id
)
SELECT
    gov_action_proposal.id,
    encode(creator_tx.hash, 'hex'),
    gov_action_proposal.index,
    gov_action_proposal.type::text,
    COALESCE(
        case when gov_action_proposal.type = 'TreasuryWithdrawals' then
            (
                select json_agg(
                    jsonb_build_object(
                        'receivingAddress', stake_address.view,
                        'amount', treasury_withdrawal.amount
                    )
                )
                from treasury_withdrawal
                left join stake_address
                    on stake_address.id = treasury_withdrawal.stake_address_id
                where treasury_withdrawal.gov_action_proposal_id = gov_action_proposal.id
            )
            when gov_action_proposal.type::text = 'InfoAction' then
            json_build_object('data', gov_action_proposal.description)

            when gov_action_proposal.type::text = 'HardForkInitiation' then
            json_build_object(
                'major', (gov_action_proposal.description->'contents'->1->>'major')::int, 
                'minor', (gov_action_proposal.description->'contents'->1->>'minor')::int
            )

            when gov_action_proposal.type::text = 'NoConfidence' then
            json_build_object('data', gov_action_proposal.description->'contents')
    
            when gov_action_proposal.type::text = 'ParameterChange' then
            json_build_object('data', gov_action_proposal.description->'contents')

            when gov_action_proposal.type::text = 'NewConstitution' then
            json_build_object(
                'anchor', gov_action_proposal.description->'contents'->1->'anchor'
            )
            when gov_action_proposal.type::text = 'NewCommittee' then
            (
                SELECT
                    json_build_object(
                        'tag', pd.tag,
                        'members', em.enriched_members,
                        'membersToBeRemoved', mtr.members_to_be_removed,
                        'threshold', pd.threshold::float
                    )
                FROM
                    parsed_description pd
                JOIN
                    members_to_be_removed mtr ON pd.id = mtr.id
                JOIN
                    enriched_current_members em ON pd.id = em.id
                WHERE
                    pd.id = gov_action_proposal.id
            )
        else
            null
        end
    , '{}'::json) as description,
    CASE
        WHEN meta.network_name::text = 'mainnet' OR meta.network_name::text = 'preprod' THEN
            latest_epoch.start_time + (gov_action_proposal.expiration - latest_epoch.no)::bigint * INTERVAL '5 days' 
        ELSE
            latest_epoch.start_time + (gov_action_proposal.expiration - latest_epoch.no)::bigint * INTERVAL '1 day' 
    END AS expiry_date,
    gov_action_proposal.expiration,
    creator_block.time,
    creator_block.epoch_no,
    voting_anchor.url,
    encode(voting_anchor.data_hash, 'hex'),
    jsonb_set(
        ROW_TO_JSON(proposal_params)::jsonb,
        '{cost_model}', 
        CASE
            WHEN cost_model.id IS NOT NULL THEN
                ROW_TO_JSON(cost_model)::jsonb
            ELSE
                'null'::jsonb
        END
    ) AS proposal_params,
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
    LEFT JOIN cost_model AS cost_model ON proposal_params.cost_model_id = cost_model.id
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
            voting_procedure AS vp
        WHERE 
            vp.committee_voter IS NOT NULL
            AND (vp.tx_id, vp.committee_voter, vp.gov_action_proposal_id) IN (
                SELECT MAX(tx_id), committee_voter, gov_action_proposal_id
                FROM voting_procedure
                WHERE committee_voter IS NOT NULL
                GROUP BY committee_voter, gov_action_proposal_id
            )
        GROUP BY 
            gov_action_proposal_id
    ) vp_by_cc
    ON gov_action_proposal.id = vp_by_cc.gov_action_proposal_id
    LEFT JOIN LatestDrepDistr ldd_cc ON ldd_cc.hash_id = voting_procedure.committee_voter
        AND ldd_cc.rn = 1
    LEFT JOIN gov_action_proposal AS prev_gov_action ON gov_action_proposal.prev_gov_action_proposal = prev_gov_action.id
    LEFT JOIN tx AS prev_gov_action_tx ON prev_gov_action.tx_id = prev_gov_action_tx.id
WHERE
  (COALESCE(?, '') = '' OR
   off_chain_vote_gov_action_data.title ILIKE ? OR
   off_chain_vote_gov_action_data.abstract ILIKE ? OR
   off_chain_vote_gov_action_data.motivation ILIKE ? OR
   off_chain_vote_gov_action_data.rationale ILIKE ? OR
   concat(encode(creator_tx.hash, 'hex'), '#', gov_action_proposal.index) ILIKE ?)
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