SELECT
   encode(dh.raw, 'hex'),
   va.url,
   encode(va.data_hash, 'hex'),
   dr_deposit.deposit
FROM drep_hash dh
JOIN (
  SELECT dr.id, dr.drep_hash_id, dr.deposit,
         ROW_NUMBER() OVER(PARTITION BY dr.drep_hash_id ORDER BY dr.id DESC) AS rn
  FROM drep_registration dr
  where dr.deposit > 0
) as dr_deposit
on dr_deposit.drep_hash_id = dh.id and dr_deposit.rn = 1
LEFT JOIN (
  SELECT dr.id, dr.drep_hash_id, dr.voting_anchor_id,
         ROW_NUMBER() OVER(PARTITION BY dr.drep_hash_id ORDER BY dr.id DESC) AS rn
  FROM drep_registration dr
) as dr_voting_anchor
on dr_voting_anchor.drep_hash_id = dh.id and dr_voting_anchor.rn = 1
left JOIN voting_anchor va ON va.id = dr_voting_anchor.voting_anchor_id

