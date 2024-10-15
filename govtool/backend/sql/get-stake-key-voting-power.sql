SELECT COALESCE(SUM(utxo_view.value::numeric), 0) + COALESCE(SUM(reward_rest.amount), 0) AS total_value,
       encode(stake_address.hash_raw, 'hex')
FROM stake_address
JOIN utxo_view ON utxo_view.stake_address_id = stake_address.id
LEFT JOIN reward_rest ON reward_rest.addr_id = stake_address.id
WHERE reward_rest.earned_epoch IS NULL
WHERE stake_address.hash_raw = decode(?, 'hex')
GROUP BY stake_address.hash_raw;