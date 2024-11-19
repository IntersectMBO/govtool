select sum(uv.value) as amount
from utxo_view uv
join delegation_vote dv on uv.stake_address_id = dv.addr_id
join drep_hash dh on dv.drep_hash_id = dh.id
where dh.raw = decode(?,'hex')
and dv.cert_index != 0