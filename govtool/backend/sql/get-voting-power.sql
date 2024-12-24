WITH LatestDelegationVote AS (
  SELECT 
    addr_id,
    MAX(id) AS latest_vote_id
  FROM 
    delegation_vote
  GROUP BY 
    addr_id
)
SELECT 
  SUM(uv.value) AS total_value
FROM 
  utxo_view uv
JOIN 
  stake_address sa ON sa.id = uv.stake_address_id
JOIN 
  LatestDelegationVote ldv ON uv.stake_address_id = ldv.addr_id
JOIN 
  delegation_vote dv ON dv.id = ldv.latest_vote_id
WHERE 
  dv.drep_hash_id = (SELECT id FROM drep_hash WHERE raw = decode(?,'hex'))
