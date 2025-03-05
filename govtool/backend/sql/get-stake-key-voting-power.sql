WITH RewardRest AS (
  SELECT 
    SUM(amount) AS amount,
    addr_id
  FROM
    reward_rest
  GROUP BY
    addr_id
),
Reward AS (
  SELECT
    SUM(amount) AS amount,
    addr_id
  FROM
    reward
  GROUP BY
    addr_id
),
Balance AS (
  SELECT
    COALESCE(SUM(uv.value), 0) AS amount,
    sa.id AS addr_id,
    encode(sa.hash_raw, 'hex') AS addr_raw
  FROM
    stake_address sa
  JOIN utxo_view uv ON uv.stake_address_id = sa.id
  GROUP BY
    addr_id,
    addr_raw
),
Withdrawal AS (
  SELECT
    COALESCE(SUM(w.amount), 0) AS withdrawal_amount,
    w.addr_id
  FROM
    withdrawal w
  GROUP BY
    w.addr_id
)
SELECT
  (COALESCE(rr.amount, 0) + COALESCE(r.amount, 0) + COALESCE(b.amount, 0) - 
  -- records in rewards tables might be missing for some epochs
  -- so we need to check if the sum of rewards is greater than the sum of withdrawals before subtracting
    CASE 
      WHEN COALESCE(rr.amount, 0) + COALESCE(r.amount, 0) > COALESCE(w.withdrawal_amount, 0) 
      THEN COALESCE(w.withdrawal_amount, 0) 
      ELSE 0 
    END
  ) AS total_balance,
  b.addr_raw::text AS stake_address
FROM
  Balance b
LEFT JOIN
  RewardRest rr ON rr.addr_id = b.addr_id
LEFT JOIN
  Reward r ON r.addr_id = rr.addr_id
LEFT JOIN
  Withdrawal w ON w.addr_id = b.addr_id
WHERE
  b.addr_id = (SELECT id FROM stake_address WHERE hash_raw = decode(?, 'hex'))
GROUP BY
  b.addr_raw,
  rr.amount,
  r.amount,
  b.amount,
  w.withdrawal_amount