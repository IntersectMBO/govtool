WITH context AS (
    SELECT
        gov_action_proposal.id AS proposal_db_id,
        off_chain_vote_data.json->>'surveyTxId' AS survey_tx_id,
        survey_meta.json->'surveyDetails' AS survey_details
    FROM gov_action_proposal
    JOIN tx AS creator_tx ON creator_tx.id = gov_action_proposal.tx_id
    LEFT JOIN voting_anchor ON voting_anchor.id = gov_action_proposal.voting_anchor_id
    LEFT JOIN off_chain_vote_data ON off_chain_vote_data.voting_anchor_id = voting_anchor.id
    LEFT JOIN tx survey_tx
      ON encode(survey_tx.hash, 'hex') = LOWER(off_chain_vote_data.json->>'surveyTxId')
    LEFT JOIN tx_metadata survey_meta
      ON survey_meta.tx_id = survey_tx.id
     AND survey_meta.key = 17
     AND survey_meta.json ? 'surveyDetails'
    WHERE encode(creator_tx.hash, 'hex') = ?
      AND gov_action_proposal.index = ?
    LIMIT 1
),
LatestDrepDistr AS (
    SELECT
        *,
        ROW_NUMBER() OVER (PARTITION BY hash_id ORDER BY epoch_no DESC) AS rn
    FROM drep_distr
),
responses AS (
    SELECT
        encode(drep_hash.raw, 'hex') AS response_credential,
        tx_metadata.id AS metadata_id,
        block.slot_no AS slot_no,
        tx.block_index AS tx_index,
        tx_metadata.json->'surveyResponse' AS survey_response,
        COALESCE(ldd.amount, 0) AS drep_voting_power
    FROM context
    JOIN tx_metadata
      ON tx_metadata.key = 17
     AND tx_metadata.json ? 'surveyResponse'
     AND LOWER(tx_metadata.json->'surveyResponse'->>'surveyTxId') = LOWER(context.survey_tx_id)
    JOIN tx ON tx.id = tx_metadata.tx_id
    JOIN block ON block.id = tx.block_id
    JOIN voting_procedure
      ON voting_procedure.tx_id = tx.id
     AND voting_procedure.gov_action_proposal_id = context.proposal_db_id
     AND voting_procedure.drep_voter IS NOT NULL
    JOIN drep_hash ON drep_hash.id = voting_procedure.drep_voter
    LEFT JOIN LatestDrepDistr ldd
      ON ldd.hash_id = voting_procedure.drep_voter
     AND ldd.rn = 1
),
latest AS (
    SELECT DISTINCT ON (response_credential)
        *
    FROM responses
    ORDER BY
        response_credential,
        slot_no DESC,
        tx_index DESC,
        metadata_id DESC
),
question_counts AS (
    SELECT
        answer->>'questionId' AS question_id,
        COUNT(*)::bigint AS answers_count,
        SUM(
            CASE
                WHEN ? = 'StakeBased' THEN drep_voting_power
                ELSE 1
            END
        )::bigint AS weighted_count
    FROM latest
    JOIN LATERAL jsonb_array_elements(
        CASE
            WHEN jsonb_typeof(latest.survey_response->'answers') = 'array'
            THEN latest.survey_response->'answers'
            ELSE '[]'::jsonb
        END
    ) AS answer ON true
    GROUP BY answer->>'questionId'
),
method_results AS (
    SELECT
        COALESCE(
            jsonb_agg(
                jsonb_build_object(
                    'questionId', q->>'questionId',
                    'question', q->>'question',
                    'methodType', q->>'methodType',
                    'count', COALESCE(question_counts.answers_count, 0),
                    'weightedCount', COALESCE(question_counts.weighted_count, 0)
                )
            ),
            '[]'::jsonb
        ) AS results
    FROM context
    LEFT JOIN LATERAL jsonb_array_elements(context.survey_details->'questions') AS q ON true
    LEFT JOIN question_counts ON question_counts.question_id = q->>'questionId'
)
SELECT
    jsonb_build_object(
        'surveyTxId', context.survey_tx_id,
        'totals', jsonb_build_object(
            'totalSeen', COALESCE((SELECT COUNT(*) FROM responses), 0),
            'valid', COALESCE((SELECT COUNT(*) FROM latest), 0),
            'invalid', GREATEST(
                COALESCE((SELECT COUNT(*) FROM responses), 0)
                - COALESCE((SELECT COUNT(*) FROM latest), 0),
                0
            ),
            'deduped', GREATEST(
                COALESCE((SELECT COUNT(*) FROM responses), 0)
                - COALESCE((SELECT COUNT(*) FROM latest), 0),
                0
            ),
            'uniqueResponders', COALESCE((SELECT COUNT(*) FROM latest), 0)
        ),
        'roleResults', jsonb_build_array(
            jsonb_build_object(
                'responderRole', 'DRep',
                'weightingMode', ?,
                'totals', jsonb_build_object(
                    'totalSeen', COALESCE((SELECT COUNT(*) FROM responses), 0),
                    'valid', COALESCE((SELECT COUNT(*) FROM latest), 0),
                    'invalid', GREATEST(
                        COALESCE((SELECT COUNT(*) FROM responses), 0)
                        - COALESCE((SELECT COUNT(*) FROM latest), 0),
                        0
                    ),
                    'deduped', GREATEST(
                        COALESCE((SELECT COUNT(*) FROM responses), 0)
                        - COALESCE((SELECT COUNT(*) FROM latest), 0),
                        0
                    ),
                    'uniqueResponders', COALESCE((SELECT COUNT(*) FROM latest), 0)
                ),
                'methodResults', COALESCE(method_results.results, '[]'::jsonb)
            )
        ),
        'errors', '[]'::jsonb
    ) AS survey_tally
FROM context
CROSS JOIN method_results;
