SELECT DISTINCT ON (raw)
  view, 
  encode(raw, 'hex') AS hash_raw,
  COALESCE(dd.amount, 0) AS voting_power
FROM drep_hash dh
LEFT JOIN drep_distr dd ON dh.id = dd.hash_id AND dd.epoch_no = (SELECT MAX(no) from epoch)
WHERE view = ? OR encode(raw, 'hex') = ?