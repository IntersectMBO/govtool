select DISTINCT ON (voting_procedure.gov_action_proposal_id, voting_procedure.drep_voter) voting_procedure.gov_action_proposal_id, concat(encode(tx.hash,'hex'),'#',gov_action_proposal.index), encode(drep_hash.raw, 'hex'), voting_procedure.vote::text, voting_anchor.url, encode(voting_anchor.data_hash, 'hex')
from voting_procedure
join gov_action_proposal
on gov_action_proposal.id = voting_procedure.gov_action_proposal_id
join drep_hash
on drep_hash.id = voting_procedure.drep_voter
left join voting_anchor
on voting_anchor.id = voting_procedure.voting_anchor_id
join tx
on tx.id = gov_action_proposal.tx_id
where drep_hash.raw = decode(?, 'hex')
order by voting_procedure.gov_action_proposal_id, voting_procedure.drep_voter, voting_procedure.id desc


