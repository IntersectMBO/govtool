WITH DRepId AS (
    SELECT
        decode(?, 'hex') AS raw
),
LatestRegistrationEntry AS (
    SELECT
        drep_registration.voting_anchor_id AS voting_anchor_id,
        deposit AS deposit
    FROM
        drep_registration
        CROSS JOIN DrepId
        JOIN drep_hash ON drep_hash.id = drep_registration.drep_hash_id
    WHERE
        drep_hash.raw = DRepId.raw
    ORDER BY
        drep_registration.tx_id DESC
    LIMIT 1
),
IsRegisteredAsDRep AS (
    SELECT
        (LatestRegistrationEntry.deposit is null or LatestRegistrationEntry.deposit > 0)
        AND LatestRegistrationEntry.voting_anchor_id IS NOT NULL AS value
    FROM
        LatestRegistrationEntry
),
IsRegisteredAsSoleVoter AS (
    SELECT
        (LatestRegistrationEntry.deposit is null or LatestRegistrationEntry.deposit > 0)
        AND LatestRegistrationEntry.voting_anchor_id IS NULL AS value
    FROM
        LatestRegistrationEntry
),
CurrentDeposit AS (
    SELECT
        GREATEST(drep_registration.deposit, 0) AS value
FROM
    drep_registration
    join drep_hash
    on drep_hash.id = drep_registration.drep_hash_id
    cross join DRepId

    WHERE
        drep_registration.deposit IS NOT NULL
        and drep_hash.raw = DRepId.raw
    ORDER BY
        drep_registration.tx_id DESC
    LIMIT 1
),
WasRegisteredAsDRep AS (
    SELECT
        (EXISTS (
                SELECT
                    *
                FROM
                    drep_registration
                    JOIN drep_hash ON drep_hash.id = drep_registration.drep_hash_id
                    CROSS JOIN DRepId
                WHERE
                    drep_hash.raw = DRepId.raw
                    AND drep_registration.voting_anchor_id IS NOT NULL)) AS value
),
WasRegisteredAsSoleVoter AS (
    SELECT
        (EXISTS (
                SELECT
                    *
                FROM
                    drep_registration
                    JOIN drep_hash ON drep_hash.id = drep_registration.drep_hash_id
                    CROSS JOIN DRepId
                WHERE
                    drep_hash.raw = DRepId.raw
                    AND drep_registration.voting_anchor_id IS NULL)) AS value
)
SELECT
    IsRegisteredAsDRep.value,
    WasRegisteredAsDRep.value,
    IsRegisteredAsSoleVoter.value,
    WasRegisteredAsSoleVoter.value,
    CurrentDeposit.value
FROM
    IsRegisteredAsDRep
    CROSS JOIN IsRegisteredAsSoleVoter
    CROSS JOIN WasRegisteredAsDRep
    CROSS JOIN WasRegisteredAsSoleVoter
    CROSS JOIN CurrentDeposit
