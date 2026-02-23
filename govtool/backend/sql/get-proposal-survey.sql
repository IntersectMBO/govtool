WITH proposal_data AS (
    SELECT
        gov_action_proposal.id AS proposal_db_id,
        gov_action_proposal.type::text AS proposal_type,
        gov_action_proposal.voting_anchor_id AS voting_anchor_id,
        creator_block.slot_no AS creator_slot,
        meta.network_name::text AS network_name,
        COALESCE(latest_epoch_param.gov_action_lifetime, 0) AS gov_action_lifetime
    FROM gov_action_proposal
    JOIN tx AS creator_tx ON creator_tx.id = gov_action_proposal.tx_id
    JOIN block AS creator_block ON creator_block.id = creator_tx.block_id
    CROSS JOIN meta
    LEFT JOIN LATERAL (
        SELECT ep.gov_action_lifetime
        FROM epoch_param ep
        ORDER BY ep.epoch_no DESC
        LIMIT 1
    ) latest_epoch_param ON true
    WHERE encode(creator_tx.hash, 'hex') = ?
      AND gov_action_proposal.index = ?
    LIMIT 1
),
survey_link AS (
    SELECT
        proposal_data.*,
        off_chain_vote_data.json AS anchor_json,
        off_chain_vote_data.json->>'kind' AS kind,
        off_chain_vote_data.json->'surveyRef'->>'surveyTxId' AS survey_tx_id,
        off_chain_vote_data.json->'surveyRef'->>'surveyHash' AS survey_hash
    FROM proposal_data
    LEFT JOIN voting_anchor ON voting_anchor.id = proposal_data.voting_anchor_id
    LEFT JOIN off_chain_vote_data ON off_chain_vote_data.voting_anchor_id = voting_anchor.id
),
survey_payload AS (
    SELECT
        survey_link.*,
        survey_meta.json->'surveyDetails' AS survey_details,
        CASE
            WHEN (survey_meta.json->'surveyDetails'->'lifecycle'->>'startSlot') ~ '^[0-9]+$'
            THEN (survey_meta.json->'surveyDetails'->'lifecycle'->>'startSlot')::bigint
            ELSE NULL
        END AS lifecycle_start_slot,
        CASE
            WHEN (survey_meta.json->'surveyDetails'->'lifecycle'->>'endSlot') ~ '^[0-9]+$'
            THEN (survey_meta.json->'surveyDetails'->'lifecycle'->>'endSlot')::bigint
            ELSE NULL
        END AS lifecycle_end_slot
    FROM survey_link
    LEFT JOIN tx survey_tx
      ON encode(survey_tx.hash, 'hex') = LOWER(survey_link.survey_tx_id)
    LEFT JOIN tx_metadata survey_meta
      ON survey_meta.tx_id = survey_tx.id
     AND survey_meta.key = 17
     AND survey_meta.json ? 'surveyDetails'
)
SELECT
    jsonb_build_object(
        'linked', (survey_tx_id IS NOT NULL AND survey_hash IS NOT NULL),
        'actionLifecycle', jsonb_build_object(
            'startSlot', creator_slot,
            'endSlot', creator_slot + (
                gov_action_lifetime
                * CASE WHEN network_name IN ('mainnet', 'preprod') THEN 432000 ELSE 86400 END
            )
        ),
        'surveyRef', CASE
            WHEN survey_tx_id IS NOT NULL AND survey_hash IS NOT NULL
            THEN jsonb_build_object(
                'surveyTxId', survey_tx_id,
                'surveyHash', survey_hash
            )
            ELSE NULL
        END,
        'computedSurveyHash', survey_hash,
        'linkValidation', jsonb_build_object(
            'valid',
            (
                proposal_type = 'InfoAction'
                AND kind = 'cardano-governance-survey-link'
                AND survey_tx_id IS NOT NULL
                AND survey_hash IS NOT NULL
                AND survey_details IS NOT NULL
            ),
            'errors',
            to_jsonb(array_remove(ARRAY[
                CASE WHEN proposal_type <> 'InfoAction'
                    THEN 'Survey linkage is only valid for InfoAction proposals.' END,
                CASE WHEN kind IS DISTINCT FROM 'cardano-governance-survey-link'
                    THEN 'Anchor metadata kind is not cardano-governance-survey-link.' END,
                CASE WHEN survey_tx_id IS NULL OR survey_hash IS NULL
                    THEN 'Missing surveyRef.surveyTxId or surveyRef.surveyHash in anchor metadata.' END,
                CASE WHEN survey_tx_id IS NOT NULL AND survey_details IS NULL
                    THEN 'Referenced surveyTxId has no label 17 surveyDetails payload.' END
            ], NULL))
        ),
        'surveyDetails', survey_details,
        'surveyDetailsValidation', jsonb_build_object(
            'valid',
            (
                survey_details IS NOT NULL
                AND lifecycle_start_slot IS NOT NULL
                AND lifecycle_end_slot IS NOT NULL
                AND lifecycle_start_slot = creator_slot
                AND lifecycle_end_slot = creator_slot + (
                    gov_action_lifetime
                    * CASE WHEN network_name IN ('mainnet', 'preprod') THEN 432000 ELSE 86400 END
                )
            ),
            'errors',
            to_jsonb(array_remove(ARRAY[
                CASE WHEN survey_details IS NULL
                    THEN 'Missing surveyDetails payload.' END,
                CASE WHEN survey_details IS NOT NULL
                    AND (lifecycle_start_slot IS NULL OR lifecycle_end_slot IS NULL)
                    THEN 'surveyDetails.lifecycle.startSlot/endSlot are required for linked InfoAction surveys.' END,
                CASE WHEN survey_details IS NOT NULL
                    AND lifecycle_start_slot IS NOT NULL
                    AND lifecycle_end_slot IS NOT NULL
                    AND (
                        lifecycle_start_slot <> creator_slot
                        OR lifecycle_end_slot <> creator_slot + (
                            gov_action_lifetime
                            * CASE WHEN network_name IN ('mainnet', 'preprod') THEN 432000 ELSE 86400 END
                        )
                    )
                    THEN 'surveyDetails.lifecycle must match InfoAction lifecycle (start/end slot mismatch).' END
            ], NULL))
        )
    ) AS survey_payload
FROM survey_payload;
