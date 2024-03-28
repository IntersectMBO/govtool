select coalesce(sum(utxo_view.value), 0), encode(stake_address.hash_raw, 'hex')
from stake_address
join utxo_view
on utxo_view.stake_address_id = stake_address.id
where stake_address.hash_raw = decode(?, 'hex')
group by stake_address.hash_raw
