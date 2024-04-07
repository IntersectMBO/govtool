select DISTINCT ON (voting_procedure.gov_action_proposal_id, voting_procedure.drep_voter) voting_procedure.gov_action_proposal_id, concat(encode(gov_action_tx.hash,'hex'),'#',gov_action_proposal.index), encode(drep_hash.raw, 'hex'), voting_procedure.vote::text, voting_anchor.url, encode(voting_anchor.data_hash, 'hex'), block.epoch_no as epoch_no, block.time as time, encode(vote_tx.hash, 'hex') as vote_tx_hash
join gov_action_proposal
on gov_action_proposal.id = voting_procedure.gov_action_proposal_id
join drep_hash
on drep_hash.id = voting_procedure.drep_voter
left join voting_anchor
on voting_anchor.id = voting_procedure.voting_anchor_id
join tx as gov_action_tx
on gov_action_tx.id = gov_action_proposal.tx_id
join tx as vote_tx
on vote_tx.id = voting_procedure.tx_id
join block
on block.id = gov_action_tx.block_id
where drep_hash.raw = decode(?, 'hex')
order by voting_procedure.gov_action_proposal_id, voting_procedure.drep_voter, voting_procedure.id desc
