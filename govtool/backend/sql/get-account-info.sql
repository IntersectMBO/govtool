SELECT
  sa.id,
  sa.view,
  CASE
    WHEN sa.script_hash IS NOT NULL THEN true
    ELSE false
  END AS is_script_based,
  CASE
    WHEN (
      SELECT COALESCE(MAX(epoch_no), 0)
      FROM stake_registration sr
      WHERE sr.addr_id = sa.id
    ) > (
      SELECT COALESCE(MAX(epoch_no), 0)
      FROM stake_deregistration sd
      WHERE sd.addr_id = sa.id
    ) THEN true
    ELSE false
  END AS is_registered
FROM
  stake_address sa
WHERE
  sa.hash_raw = decode(?, 'hex');
