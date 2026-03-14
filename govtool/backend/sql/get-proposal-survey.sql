WITH proposal_data AS (
    SELECT
        gov_action_proposal.type::text AS proposal_type,
        encode(creator_tx.hash, 'hex') AS proposal_tx_id,
        gov_action_proposal.index AS proposal_index,
        gov_action_proposal.expiration AS expiration_epoch,
        off_chain_vote_data.json AS anchor_json,
        off_chain_vote_data.json->>'kind' AS kind,
        off_chain_vote_data.json->>'surveyTxId' AS survey_tx_id
    FROM gov_action_proposal
    JOIN tx AS creator_tx ON creator_tx.id = gov_action_proposal.tx_id
    LEFT JOIN voting_anchor ON voting_anchor.id = gov_action_proposal.voting_anchor_id
    LEFT JOIN off_chain_vote_data ON off_chain_vote_data.voting_anchor_id = voting_anchor.id
    WHERE encode(creator_tx.hash, 'hex') = ?
      AND gov_action_proposal.index = ?
    LIMIT 1
),
survey_payload AS (
    SELECT
        proposal_data.*,
        survey_meta.json->'surveyDetails' AS survey_details,
        CASE
            WHEN (survey_meta.json->'surveyDetails'->>'endEpoch') ~ '^[0-9]+$'
            THEN (survey_meta.json->'surveyDetails'->>'endEpoch')::bigint
            ELSE NULL
        END AS survey_end_epoch,
        survey_meta.json->'surveyDetails'->'roleWeighting' AS role_weighting,
        CASE
            WHEN proposal_type IN ('TreasuryWithdrawals', 'NewConstitution')
            THEN '["DRep","CC"]'::jsonb
            ELSE '["DRep","SPO","CC"]'::jsonb
        END AS action_eligibility
    FROM proposal_data
    LEFT JOIN tx survey_tx
      ON encode(survey_tx.hash, 'hex') = LOWER(proposal_data.survey_tx_id)
    LEFT JOIN tx_metadata survey_meta
      ON survey_meta.tx_id = survey_tx.id
     AND survey_meta.key = 17
     AND survey_meta.json ? 'surveyDetails'
)
SELECT
    jsonb_build_object(
        'linked', survey_tx_id IS NOT NULL,
        'surveyTxId', survey_tx_id,
        'linkValidation', jsonb_build_object(
            'valid',
            (
                kind = 'cardano-governance-survey-link'
                AND survey_tx_id IS NOT NULL
                AND survey_details IS NOT NULL
            )
            AND (
                role_weighting IS NOT NULL
                AND (
                    SELECT COUNT(*)
                    FROM jsonb_object_keys(role_weighting) AS role_key
                    WHERE action_eligibility ? role_key
                ) > 0
            ),
            'errors',
            to_jsonb(array_remove(ARRAY[
                CASE WHEN kind IS DISTINCT FROM 'cardano-governance-survey-link'
                    THEN 'Anchor metadata kind is not cardano-governance-survey-link.' END,
                CASE WHEN survey_tx_id IS NULL
                    THEN 'Missing surveyTxId in anchor metadata.' END,
                CASE WHEN survey_tx_id IS NOT NULL AND survey_details IS NULL
                    THEN 'Referenced surveyTxId has no label 17 surveyDetails payload.' END,
                CASE WHEN role_weighting IS NOT NULL
                    AND (
                        SELECT COUNT(*)
                        FROM jsonb_object_keys(role_weighting) AS role_key
                        WHERE action_eligibility ? role_key
                    ) = 0
                    THEN 'Linked survey has no eligible roles after governance action filtering.' END
            ], NULL)),
            'actionEligibility', action_eligibility,
            'linkedRoleWeighting',
            COALESCE(
                (
                    SELECT jsonb_object_agg(role_key, role_weighting->role_key)
                    FROM jsonb_object_keys(role_weighting) AS role_key
                    WHERE action_eligibility ? role_key
                ),
                'null'::jsonb
            ),
            'linkedActionId', jsonb_build_object(
                'txId', proposal_tx_id,
                'govActionIx', proposal_index
            )
        ),
        'surveyDetails', survey_details,
        'surveyDetailsValidation', jsonb_build_object(
            'valid',
            (
                survey_details IS NOT NULL
                AND role_weighting IS NOT NULL
                AND jsonb_typeof(role_weighting) = 'object'
                AND jsonb_object_length(role_weighting) > 0
                AND survey_end_epoch IS NOT NULL
                AND survey_end_epoch = expiration_epoch
            ),
            'errors',
            to_jsonb(array_remove(ARRAY[
                CASE WHEN survey_details IS NULL
                    THEN 'Missing surveyDetails payload.' END,
                CASE WHEN survey_details IS NOT NULL
                    AND (
                        role_weighting IS NULL
                        OR jsonb_typeof(role_weighting) <> 'object'
                        OR jsonb_object_length(role_weighting) = 0
                    )
                    THEN 'surveyDetails.roleWeighting must be a non-empty object.' END,
                CASE WHEN survey_details IS NOT NULL
                    AND survey_end_epoch IS NULL
                    THEN 'surveyDetails.endEpoch is required.' END,
                CASE WHEN survey_end_epoch IS NOT NULL
                    AND survey_end_epoch <> expiration_epoch
                    THEN 'surveyDetails.endEpoch must exactly match the governance action expiration epoch.' END
            ], NULL))
        )
    ) AS survey_payload
FROM survey_payload;
