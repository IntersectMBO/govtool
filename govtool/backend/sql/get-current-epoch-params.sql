SELECT
    jsonb_set(
        ROW_TO_JSON(epoch_param)::jsonb,
        '{cost_model}', 
        CASE
            WHEN cost_model.id IS NOT NULL THEN
                ROW_TO_JSON(cost_model)::jsonb
            ELSE
                'null'::jsonb
        END
    ) AS epoch_param
FROM
    epoch_param
LEFT JOIN
    cost_model ON epoch_param.cost_model_id = cost_model.id
ORDER BY
    epoch_no DESC
LIMIT 1;
