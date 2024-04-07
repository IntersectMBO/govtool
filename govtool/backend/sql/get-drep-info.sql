WITH DRepId AS (
    SELECT
        decode(?, 'hex') AS raw
),
LatestRegistrationEntry AS (
    SELECT
        drep_registration.voting_anchor_id AS voting_anchor_id,
        deposit AS deposit
        tx.hash as tx_hash
    FROM
        drep_registration
        CROSS JOIN DrepId
        JOIN drep_hash ON drep_hash.id = drep_registration.drep_hash_id
        JOIN tx ON tx.id = drep_registration.tx_id
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
), CurrentMetadata AS (
    SELECT voting_anchor.url as url, encode(voting_anchor.data_hash, 'hex') as data_hash
    FROM LatestRegistrationEntry
    LEFT JOIN voting_anchor
    ON voting_anchor.id = LatestRegistrationEntry.voting_anchor_id
    LIMIT 1
), CurrentVotingPower AS (
    SELECT amount as amount
    FROM drep_hash
    JOIN DRepId
    ON drep_hash.raw = DRepId.raw
    LEFT JOIN drep_distr
    ON drep_distr.hash_id = drep_hash.id
    ORDER BY drep_distr.epoch_no DESC
    LIMIT 1
)
SELECT
    IsRegisteredAsDRep.value,
    WasRegisteredAsDRep.value,
    IsRegisteredAsSoleVoter.value,
    WasRegisteredAsSoleVoter.value,
    CurrentDeposit.value,
    CurrentMetadata.url,
    CurrentMetadata.data_hash,
    CurrentVotingPower.amount,
    LatestRegistrationEntry.tx_hash
FROM
    IsRegisteredAsDRep
    CROSS JOIN IsRegisteredAsSoleVoter
    CROSS JOIN WasRegisteredAsDRep
    CROSS JOIN WasRegisteredAsSoleVoter
    CROSS JOIN CurrentDeposit
    CROSS JOIN CurrentMetadata
    CROSS JOIN CurrentVotingPower
