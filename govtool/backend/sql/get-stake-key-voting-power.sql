SELECT
  COALESCE(SUM(tx_out.value), 0) AS voting_power,
  stake_address.id as addr_id
FROM
  stake_address
JOIN
  tx_out on tx_out.stake_address_id = stake_address.id
LEFT JOIN
  tx_in ON tx_in.tx_out_id = tx_out.id AND tx_in.tx_out_index = tx_out.index
WHERE
  stake_address.hash_raw = decode(?, 'hex') AND tx_in.id IS NULL
GROUP BY
  stake_address.id;