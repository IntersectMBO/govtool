SELECT
    amount
FROM
    drep_distr
WHERE
    hash_id = (SELECT id FROM drep_hash WHERE raw = decode($1,'hex'))
ORDER BY epoch_no DESC LIMIT 1