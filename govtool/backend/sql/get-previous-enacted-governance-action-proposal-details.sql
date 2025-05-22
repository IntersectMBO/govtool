SELECT
  gap.id,
  tx_id,
  index,
  description,
  encode(hash, 'hex') AS hash
FROM
  gov_action_proposal gap
JOIN
  tx ON gap.tx_id = tx.id
WHERE
  gap.type = ? AND gap.enacted_epoch IS NOT NULL
ORDER BY
  gap.id DESC
LIMIT 1;