select encode(stake_address.hash_raw, 'hex'), voting_procedure.vote::text, gov_action_proposal.id
from gov_action_proposal
join voting_procedure
on voting_procedure.gov_action_proposal_id = gov_action_proposal.id
join delegation_vote
on delegation_vote.drep_hash_id = voting_procedure.drep_voter
join stake_address
on stake_address.id = delegation_vote.addr_id
where gov_action_proposal.id in ?

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