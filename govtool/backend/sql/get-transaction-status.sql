SELECT 
    EXISTS (SELECT 1 FROM tx WHERE tx.hash = decode(?, 'hex')) AS tx_exists,
    COALESCE(
        (SELECT json_agg(voting_procedure.*)
            FROM voting_procedure
            JOIN tx ON voting_procedure.tx_id = tx.id
            WHERE tx.hash = decode(?, 'hex')
        ), '[]'::json
    ) AS voting_procedures;
