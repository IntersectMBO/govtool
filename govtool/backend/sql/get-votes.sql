SELECT DISTINCT ON (voting_procedure.gov_action_proposal_id, voting_procedure.drep_voter)
  voting_procedure.gov_action_proposal_id, 
  CONCAT(encode(gov_action_tx.hash,'hex'),'#',gov_action_proposal.index),
  encode(drep_hash.raw, 'hex'),
  LOWER(voting_procedure.vote::text),
  voting_anchor.url,
  encode(voting_anchor.data_hash, 'hex'),
  block.epoch_no AS epoch_no,
  block.time AS time,
  encode(vote_tx.hash, 'hex') AS vote_tx_hash
FROM voting_procedure
JOIN gov_action_proposal
ON gov_action_proposal.id = voting_procedure.gov_action_proposal_id
JOIN drep_hash
ON drep_hash.id = voting_procedure.drep_voter
LEFT JOIN voting_anchor ON voting_anchor.id = voting_procedure.voting_anchor_id
JOIN tx AS gov_action_tx ON gov_action_tx.id = gov_action_proposal.tx_id
JOIN tx AS vote_tx ON vote_tx.id = voting_procedure.tx_id
JOIN block ON block.id = vote_tx.block_id
WHERE drep_hash.raw = decode(?, 'hex')
ORDER BY voting_procedure.gov_action_proposal_id, voting_procedure.drep_voter, voting_procedure.id DESC
