WITH DRepId AS (
    SELECT decode(?, 'hex') as raw
), IsRegisteredAsDRep AS (
    SELECT (deposit>0) as value,
           deposit as deposit
    FROM drep_registration
    JOIN drep_hash
    ON drep_hash.id = drep_registration.drep_hash_id
    CROSS JOIN DRepId
    WHERE drep_hash.raw = DRepId.raw
    and deposit is not null
    and drep_registration.voting_anchor_id is not null
    ORDER BY drep_registration.tx_id DESC
    LIMIT 1
), WasRegisteredAsDRep AS (
    select (EXISTS (
        SELECT *
        FROM drep_registration
        JOIN drep_hash
        ON drep_hash.id = drep_registration.drep_hash_id
        CROSS JOIN DRepId
        WHERE drep_hash.raw = DRepId.raw
        and drep_registration.deposit > 0
        and drep_registration.voting_anchor_id is not null
    )) as value
), IsRegisteredAsSoleVoter AS (
    SELECT (deposit>0) as value,
           deposit as deposit
    FROM drep_registration
    JOIN drep_hash
    ON drep_hash.id = drep_registration.drep_hash_id
    CROSS JOIN DRepId
    WHERE drep_hash.raw = DRepId.raw
    and deposit is not null
    and drep_registration.voting_anchor_id is null
    ORDER BY drep_registration.tx_id DESC
    LIMIT 1
), WasRegisteredAsSoleVoter AS (
    select (EXISTS (
        SELECT *
        FROM drep_registration
        JOIN drep_hash
        ON drep_hash.id = drep_registration.drep_hash_id
        CROSS JOIN DRepId
        WHERE drep_hash.raw = DRepId.raw
        and drep_registration.deposit > 0
        and drep_registration.voting_anchor_id is null
    )) as value
)
SELECT
    IsRegisteredAsDrep.value,
    WasRegisteredAsDRep.value,
    IsRegisteredAsSoleVoter.value,
    WasRegisteredAsSoleVoter.value,
    coalesce(IsRegisteredAsDRep.deposit, IsRegisteredAsSoleVoter.deposit)
FROM WasRegisteredAsDRep
LEFT JOIN IsRegisteredAsDRep
ON 1=1
LEFT JOIN WasRegisteredAsSoleVoter
ON 1=1
LEFT JOIN IsRegisteredAsSoleVoter
ON 1=1