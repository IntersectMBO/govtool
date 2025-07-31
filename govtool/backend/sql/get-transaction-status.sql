SELECT 
    EXISTS (SELECT 1 FROM tx WHERE tx.hash = decode(?, 'hex')) AS tx_exists,
    COALESCE(
        (SELECT json_agg(voting_procedure.*)
            FROM voting_procedure
            JOIN tx ON voting_procedure.tx_id = tx.id
            WHERE tx.hash = decode(?, 'hex')
        ), '[]'::json
    ) AS voting_procedures,
    COALESCE(
        (SELECT NULLIF(jsonb_agg(
            jsonb_build_object(
                'drep_registration', to_jsonb(drep_registration.*),
                'off_chain_vote_drep_data', 
                (SELECT NULLIF(jsonb_agg(to_jsonb(off_chain_vote_drep_data)), '[]'::jsonb)
                    FROM off_chain_vote_data
                    JOIN off_chain_vote_drep_data 
                        ON off_chain_vote_drep_data.off_chain_vote_data_id = off_chain_vote_data.id
                    WHERE off_chain_vote_data.voting_anchor_id = drep_registration.voting_anchor_id
                )
            )
        ), '[]'::jsonb)
        FROM drep_registration
        JOIN tx ON drep_registration.tx_id = tx.id
        WHERE tx.hash = decode(?, 'hex')
        ), NULL
    ) AS drep_registrations
