WITH LatestExistingVotingAnchor AS (
  SELECT
    subquery.drep_registration_id,
    subquery.drep_hash_id,
    subquery.voting_anchor_id,
    subquery.url,
    subquery.metadata_hash,
    subquery.ocvd_id
  FROM (
    SELECT
      dr.id AS drep_registration_id,
      dr.drep_hash_id,
      va.id AS voting_anchor_id,
      va.url,
      encode(va.data_hash, 'hex') AS metadata_hash,
      ocvd.id AS ocvd_id,
      ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn
    FROM
      drep_registration dr
    JOIN voting_anchor va ON dr.voting_anchor_id = va.id
    JOIN off_chain_vote_data ocvd ON va.id = ocvd.voting_anchor_id
    WHERE
      ocvd.voting_anchor_id IS NOT NULL
  ) subquery
  WHERE
    subquery.rn = 1
)
SELECT DISTINCT ON (raw)
  view, 
  encode(raw, 'hex') AS hash_raw,
  COALESCE(dd.amount, 0) AS voting_power,
  ocvdd.given_name
FROM drep_hash dh
LEFT JOIN drep_distr dd ON dh.id = dd.hash_id AND dd.epoch_no = (SELECT MAX(no) from epoch)
LEFT JOIN LatestExistingVotingAnchor leva ON leva.drep_hash_id = dh.id
LEFT JOIN off_chain_vote_data ocvd ON ocvd.id = leva.ocvd_id
LEFT JOIN off_chain_vote_drep_data ocvdd ON ocvdd.off_chain_vote_data_id = ocvd.id