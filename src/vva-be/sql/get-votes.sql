select DISTINCT ON (voting_procedure.governance_action_id, voting_procedure.drep_voter) voting_procedure.governance_action_id, concat(encode(tx.hash,'hex'),'#',governance_action.index), encode(drep_hash.raw, 'hex'), voting_procedure.vote::text, voting_anchor.url, encode(voting_anchor.data_hash, 'hex')
from voting_procedure
join governance_action
on governance_action.id = voting_procedure.governance_action_id
join drep_hash
on drep_hash.id = voting_procedure.drep_voter
left join voting_anchor
on voting_anchor.id = voting_procedure.voting_anchor_id
join tx
on tx.id = governance_action.tx_id
where drep_hash.raw = decode(?, 'hex')
order by voting_procedure.governance_action_id, voting_procedure.drep_voter, voting_procedure.id desc


