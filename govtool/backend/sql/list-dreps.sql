WITH DRepDistr AS (
  SELECT
    *,
    ROW_NUMBER() OVER(PARTITION BY drep_hash.id ORDER BY drep_distr.epoch_no DESC) AS rn
  FROM drep_distr
  JOIN drep_hash
  on drep_hash.id = drep_distr.hash_id
), DRepActivity AS (
  select
    drep_activity as drep_activity,
    epoch_no as epoch_no
  from epoch_param
  where epoch_no is not null
  order by epoch_no desc
  limit 1
)

SELECT
   encode(dh.raw, 'hex'),
   dh.view,
   va.url,
   encode(va.data_hash, 'hex'),
   dr_deposit.deposit,
   DRepDistr.amount,
   (DRepActivity.epoch_no - Max(coalesce(block.epoch_no,block_first_register.epoch_no))) <= DRepActivity.drep_activity as active,
   second_to_newest_drep_registration.voting_anchor_id is not null as has_voting_anchor,
   encode(dr_voting_anchor.tx_hash, 'hex') as tx_hash
FROM drep_hash dh
JOIN (
  SELECT dr.id, dr.drep_hash_id, dr.deposit,
         ROW_NUMBER() OVER(PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn
  FROM drep_registration dr
  where dr.deposit is not null
) as dr_deposit
on dr_deposit.drep_hash_id = dh.id and dr_deposit.rn = 1
LEFT JOIN (
  SELECT dr.id, dr.drep_hash_id, dr.voting_anchor_id,
         ROW_NUMBER() OVER(PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn,
         tx.hash as tx_hash
  FROM drep_registration dr
  JOIN tx on tx.id = dr.tx_id
) as dr_voting_anchor
on dr_voting_anchor.drep_hash_id = dh.id and dr_voting_anchor.rn = 1
LEFT JOIN (
  SELECT dr.id, dr.drep_hash_id, dr.voting_anchor_id,
         ROW_NUMBER() OVER(PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn
  FROM drep_registration dr
) as second_to_newest_drep_registration
on second_to_newest_drep_registration.drep_hash_id = dh.id and second_to_newest_drep_registration.rn = 2
LEFT JOIN DRepDistr
on DRepDistr.hash_id = dh.id and DRepDistr.rn = 1
LEFT JOIN voting_anchor va ON va.id = dr_voting_anchor.voting_anchor_id
CROSS JOIN DRepActivity
LEFT JOIN voting_procedure as voting_procedure
on voting_procedure.drep_voter = dh.id
LEFT JOIN tx as tx
on tx.id = voting_procedure.tx_id
LEFT JOIN block as block
on block.id = tx.block_id
JOIN (
  SELECT dr.tx_id, dr.drep_hash_id,
         ROW_NUMBER() OVER(PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id ASC) AS rn
  FROM drep_registration dr
) as dr_first_register
on dr_first_register.drep_hash_id = dh.id and dr_first_register.rn = 1
JOIN tx as tx_first_register
on tx_first_register.id = dr_first_register.tx_id
JOIN block as block_first_register
ON block_first_register.id = tx_first_register.block_id

GROUP BY dh.raw, second_to_newest_drep_registration.voting_anchor_id, dh.view, va.url, va.data_hash, dr_deposit.deposit, DRepDistr.amount, DRepActivity.epoch_no, DRepActivity.drep_activity, dr_voting_anchor.tx_hash
