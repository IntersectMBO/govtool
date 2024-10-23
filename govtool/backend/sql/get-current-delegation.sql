select
  case
    when drep_hash.raw is NULL then NULL
    else encode(drep_hash.raw,'hex')
  end as drep_raw,
  drep_hash.view as drep_view,
  EXISTS (
    SELECT dh.has_script
    FROM drep_hash as dh
    WHERE drep_hash.raw = dh.raw
    AND dh.has_script = true
    LIMIT 1
  ) AS has_script,
  encode(tx.hash, 'hex')
from delegation_vote
join tx on tx.id = delegation_vote.tx_id
join drep_hash
on drep_hash.id = delegation_vote.drep_hash_id
join stake_address
on stake_address.id = delegation_vote.addr_id
where stake_address.hash_raw = decode(?, 'hex')
and not exists (select * from delegation_vote as dv2 where dv2.addr_id = delegation_vote.addr_id and dv2.tx_id > delegation_vote.tx_id)
limit 1;
