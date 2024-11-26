SELECT COALESCE(SUM(utxo_view.value::numeric), 0),
       encode(stake_address.hash_raw, 'hex')
FROM stake_address
JOIN utxo_view ON utxo_view.stake_address_id = stake_address.id
WHERE stake_address.hash_raw = decode(?, 'hex')
GROUP BY stake_address.hash_raw;
