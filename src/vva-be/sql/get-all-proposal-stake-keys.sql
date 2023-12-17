select encode(stake_address.hash_raw, 'hex'), voting_procedure.vote::text, governance_action.id
from governance_action
join voting_procedure
on voting_procedure.governance_action_id = governance_action.id
join delegation_vote
on delegation_vote.drep_hash_id = voting_procedure.drep_voter
join stake_address
on stake_address.id = delegation_vote.addr_id
where governance_action.id in ?

union all

select encode(stake_address.hash_raw, 'hex'), 'No', NULL
from delegation_vote
join drep_hash
on drep_hash.id = delegation_vote.id
join stake_address
on stake_address.id = delegation_vote.addr_id
where drep_hash.view = 'AlwaysNoConfidence'

union all


select encode(stake_address.hash_raw, 'hex'), 'Abstain', NULL
from delegation_vote
join drep_hash
on drep_hash.id = delegation_vote.id
join stake_address
on stake_address.id = delegation_vote.addr_id
where drep_hash.view = 'AlwaysAbstain'