SELECT COALESCE(SUM(utxo_view.value::numeric), 0) + COALESCE(reward_sum.total_reward, 0) AS total_value,
       encode(stake_address.hash_raw, 'hex')
FROM stake_address
JOIN utxo_view ON utxo_view.stake_address_id = stake_address.id
LEFT JOIN (
    SELECT addr_id, SUM(reward_rest.amount) AS total_reward
    FROM reward_rest
    GROUP BY addr_id
) AS reward_sum ON reward_sum.addr_id = stake_address.id
WHERE stake_address.hash_raw = decode(?, 'hex')
GROUP BY stake_address.hash_raw, reward_sum.total_reward;
