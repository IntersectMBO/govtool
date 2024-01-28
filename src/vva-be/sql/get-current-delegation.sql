select
  case
    when drep_hash.raw is NULL then drep_hash.view
    else encode(drep_hash.raw,'hex')
  end
from delegation_vote
join drep_hash
on drep_hash.id = delegation_vote.drep_hash_id
join stake_address
on stake_address.id = delegation_vote.addr_id
where stake_address.hash_raw = decode(?, 'hex')
and not exists (select * from delegation_vote as dv2 where dv2.addr_id = delegation_vote.addr_id and dv2.tx_id > delegation_vote.tx_id)
limit 1;