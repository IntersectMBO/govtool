WITH DRepId AS (
    SELECT
        decode(?, 'hex') AS raw
),
AllRegistrationEntries AS (
    SELECT
        drep_registration.voting_anchor_id AS voting_anchor_id,
        drep_registration.deposit AS deposit,
        tx.hash AS tx_hash,
        tx.id as tx_id
    FROM
        drep_registration
        CROSS JOIN DRepId
        JOIN drep_hash ON drep_hash.id = drep_registration.drep_hash_id
        JOIN tx ON tx.id = drep_registration.tx_id

    WHERE
        drep_hash.raw = DRepId.raw
    ORDER BY drep_registration.tx_id asc
),
LatestRegistrationEntry AS (
    SELECT
        drep_registration.voting_anchor_id AS voting_anchor_id,
        drep_registration.deposit AS deposit,
        tx.hash AS tx_hash
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
IsScriptHash AS (
    SELECT EXISTS(
        SELECT
            drep_hash.has_script
        FROM
            drep_hash
            CROSS JOIN DRepId
        WHERE
            drep_hash.raw = DRepId.raw
        AND
            drep_hash.has_script = true
) AS has_script),
IsRegisteredAsDRep AS (
    SELECT
        (LatestRegistrationEntry.deposit IS NULL
            OR LatestRegistrationEntry.deposit > 0)
        AND LatestRegistrationEntry.voting_anchor_id IS NOT NULL AS value
    FROM
        LatestRegistrationEntry
),
IsRegisteredAsSoleVoter AS (
    SELECT
        (LatestRegistrationEntry.deposit IS NULL
            OR LatestRegistrationEntry.deposit > 0)
        AND LatestRegistrationEntry.voting_anchor_id IS NULL AS value
    FROM
        LatestRegistrationEntry
),
CurrentDeposit AS (
    SELECT
        GREATEST(drep_registration.deposit, 0) AS value
FROM
    drep_registration
    JOIN drep_hash ON drep_hash.id = drep_registration.drep_hash_id
        CROSS JOIN DRepId
    WHERE
        drep_registration.deposit IS NOT NULL
        AND drep_hash.raw = DRepId.raw
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
                    AND drep_registration.deposit >= 0
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
                    AND drep_registration.deposit >= 0
                    AND drep_registration.voting_anchor_id IS NULL)) AS value
),
CurrentMetadata AS (
    SELECT
        voting_anchor.url AS url,
        encode(voting_anchor.data_hash, 'hex') AS data_hash
FROM
    LatestRegistrationEntry
    LEFT JOIN voting_anchor ON voting_anchor.id = LatestRegistrationEntry.voting_anchor_id
LIMIT 1
),
CurrentVotingPower AS (
    SELECT
        amount AS amount
    FROM
        drep_hash
        JOIN DRepId ON drep_hash.raw = DRepId.raw
        LEFT JOIN drep_distr ON drep_distr.hash_id = drep_hash.id
    ORDER BY
        drep_distr.epoch_no DESC
    LIMIT 1
),
DRepRegister AS (
    SELECT
        encode(AllRegistrationEntries.tx_hash, 'hex') as tx_hash,
        AllRegistrationEntries.tx_id
    FROM
        (SELECT 1) AS dummy
    LEFT JOIN
        AllRegistrationEntries ON AllRegistrationEntries.voting_anchor_id IS NOT NULL and not (coalesce(deposit,0) < 0)
    ORDER BY
        AllRegistrationEntries.tx_id DESC
    LIMIT 1
),
DRepRetire AS (
    SELECT
        encode(AllRegistrationEntries.tx_hash, 'hex') as tx_hash,
        AllRegistrationEntries.tx_id as tx_id
    FROM
        DRepRegister
    LEFT JOIN
        AllRegistrationEntries ON (AllRegistrationEntries.deposit < 0
            OR AllRegistrationEntries.voting_anchor_id IS NULL)
            and  AllRegistrationEntries.tx_id > DRepRegister.tx_id
    ORDER BY
        AllRegistrationEntries.tx_id asc

    LIMIT 1
),

SoleVoterRegister AS (
    SELECT
        encode(AllRegistrationEntries.tx_hash, 'hex') as tx_hash,
        AllRegistrationEntries.tx_id
    FROM
        (SELECT 1) AS dummy
    LEFT JOIN
        AllRegistrationEntries ON AllRegistrationEntries.voting_anchor_id IS NULL and not (coalesce(deposit,0) < 0)
    ORDER BY
        AllRegistrationEntries.tx_id DESC
    LIMIT 1
),
SoleVoterRetire AS (
    SELECT
        encode(AllRegistrationEntries.tx_hash, 'hex') as tx_hash
    FROM
        SoleVoterRegister
    LEFT JOIN
        AllRegistrationEntries ON (AllRegistrationEntries.deposit < 0
            OR AllRegistrationEntries.voting_anchor_id IS NOT NULL)
            AND AllRegistrationEntries.tx_id > SoleVoterRegister.tx_id
    ORDER BY
        AllRegistrationEntries.tx_id asc

    LIMIT 1
)
SELECT
    IsScriptHash.has_script,
    IsRegisteredAsDRep.value,
    WasRegisteredAsDRep.value,
    IsRegisteredAsSoleVoter.value,
    WasRegisteredAsSoleVoter.value,
    CurrentDeposit.value,
    CurrentMetadata.url,
    CurrentMetadata.data_hash,
    CurrentVotingPower.amount,
    DRepRegister.tx_hash,
    DRepRetire.tx_hash,
    SoleVoterRegister.tx_hash,
    SoleVoterRetire.tx_hash,
    off_chain_vote_drep_data.payment_address,
    off_chain_vote_drep_data.given_name,
    off_chain_vote_drep_data.objectives,
    off_chain_vote_drep_data.motivations,
    off_chain_vote_drep_data.qualifications,
    off_chain_vote_drep_data.image_url,
    off_chain_vote_drep_data.image_hash
FROM
    IsRegisteredAsDRep
    CROSS JOIN IsRegisteredAsSoleVoter
    CROSS JOIN WasRegisteredAsDRep
    CROSS JOIN WasRegisteredAsSoleVoter
    CROSS JOIN CurrentDeposit
    CROSS JOIN CurrentMetadata
    CROSS JOIN CurrentVotingPower
    CROSS JOIN DRepRegister
    CROSS JOIN DRepRetire
    CROSS JOIN SoleVoterRegister
    CROSS JOIN SoleVoterRetire
    CROSS JOIN LatestRegistrationEntry
    CROSS JOIN IsScriptHash
    LEFT JOIN off_chain_vote_data ON off_chain_vote_data.voting_anchor_id = LatestRegistrationEntry.voting_anchor_id
    LEFT JOIN off_chain_vote_drep_data ON off_chain_vote_drep_data.off_chain_vote_data_id = off_chain_vote_data.id
