WITH DRepId AS (
    SELECT decode(?, 'hex') as raw
), IsRegistered AS (
    SELECT (deposit>0) as value,
           deposit as deposit
    FROM drep_registration
    JOIN drep_hash
    ON drep_hash.id = drep_registration.drep_hash_id
    CROSS JOIN DRepId
    WHERE drep_hash.raw = DRepId.raw
    and deposit is not null
    ORDER BY drep_registration.tx_id DESC
    LIMIT 1
), WasRegistered AS (
    select (EXISTS (
        SELECT *
        FROM drep_registration
        JOIN drep_hash
        ON drep_hash.id = drep_registration.drep_hash_id
        CROSS JOIN DRepId
        WHERE drep_hash.raw = DRepId.raw
        and drep_registration.deposit > 0
    )) as value
)
SELECT IsRegistered.value, WasRegistered.value, IsRegistered.deposit
FROM WasRegistered
LEFT JOIN IsRegistered
ON 1=1