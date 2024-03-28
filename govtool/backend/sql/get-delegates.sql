SELECT delegation_vote.stake_addr
from drep
join delegation_vote
on delegation_vote.drep_id = drep.id
where drep.drep_raw = ?
