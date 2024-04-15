BEGIN;
REVOKE ALL PRIVILEGES ON ALL TABLES IN SCHEMA public FROM govtool_viewer;
REVOKE ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public FROM govtool_viewer;
REVOKE ALL PRIVILEGES ON ALL FUNCTIONS IN SCHEMA public FROM govtool_viewer;
REVOKE ALL PRIVILEGES ON SCHEMA public FROM govtool_viewer;
ALTER DEFAULT PRIVILEGES IN SCHEMA public REVOKE ALL ON SEQUENCES FROM govtool_viewer;
ALTER DEFAULT PRIVILEGES IN SCHEMA public REVOKE ALL ON TABLES FROM govtool_viewer;
ALTER DEFAULT PRIVILEGES IN SCHEMA public REVOKE ALL ON FUNCTIONS FROM govtool_viewer;
REVOKE USAGE ON SCHEMA public FROM govtool_viewer;
REVOKE ALL PRIVILEGES ON ALL TABLES IN SCHEMA govtool FROM govtool_viewer;
REVOKE ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA govtool FROM govtool_viewer;
REVOKE ALL PRIVILEGES ON ALL FUNCTIONS IN SCHEMA govtool FROM govtool_viewer;
REVOKE ALL PRIVILEGES ON SCHEMA govtool FROM govtool_viewer;
ALTER DEFAULT PRIVILEGES IN SCHEMA govtool REVOKE ALL ON SEQUENCES FROM govtool_viewer;
ALTER DEFAULT PRIVILEGES IN SCHEMA govtool REVOKE ALL ON TABLES FROM govtool_viewer;
ALTER DEFAULT PRIVILEGES IN SCHEMA govtool REVOKE ALL ON FUNCTIONS FROM govtool_viewer;
REVOKE USAGE ON SCHEMA govtool FROM govtool_viewer;
REASSIGN OWNED BY govtool_viewer TO jankun;
REVOKE govtool_viewer FROM jankun;
DROP USER govtool_viewer;
DROP SCHEMA govtool CASCADE;
CREATE ROLE govtool_viewer NOLOGIN;
CREATE SCHEMA govtool;
GRANT USAGE ON SCHEMA govtool TO govtool_viewer;
GRANT govtool_viewer TO jankun;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO govtool_viewer;
GRANT SELECT ON ALL TABLES IN SCHEMA govtool TO govtool_viewer;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA govtool TO govtool_viewer;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT
SELECT
    ON TABLES TO govtool_viewer;
ALTER DEFAULT PRIVILEGES IN SCHEMA govtool GRANT
SELECT
    ON TABLES TO govtool_viewer;
SET search_path TO govtool, public;
DROP VIEW IF EXISTS proposal_stake_key;
CREATE VIEW proposal_stake_key AS
SELECT
    encode(stake_address.hash_raw, 'hex') AS stake_key_hash,
    voting_procedure.vote::text AS vote,
    gov_action_proposal.id AS proposal_id
FROM
    gov_action_proposal
    JOIN voting_procedure ON voting_procedure.gov_action_proposal_id = gov_action_proposal.id
    JOIN delegation_vote ON delegation_vote.drep_hash_id = voting_procedure.drep_voter
    JOIN stake_address ON stake_address.id = delegation_vote.addr_id
UNION ALL
SELECT
    encode(stake_address.hash_raw, 'hex'),
    'No',
    NULL
FROM
    delegation_vote
    JOIN drep_hash ON drep_hash.id = delegation_vote.id
    JOIN stake_address ON stake_address.id = delegation_vote.addr_id
WHERE
    drep_hash.view = 'AlwaysNoConfidence'
UNION ALL
SELECT
    encode(stake_address.hash_raw, 'hex'),
    'Abstain',
    NULL
FROM
    delegation_vote
    JOIN drep_hash ON drep_hash.id = delegation_vote.id
    JOIN stake_address ON stake_address.id = delegation_vote.addr_id
WHERE
    drep_hash.view = 'AlwaysAbstain';
DROP VIEW IF EXISTS current_delegation;
CREATE VIEW current_delegation AS
SELECT
    stake_address.hash_raw AS stake_key_hash,
    encode(drep_hash.raw, 'hex') AS drep_raw,
    drep_hash.view AS drep_view,
    encode(tx.hash, 'hex') AS tx_hash
FROM
    delegation_vote
    JOIN tx ON tx.id = delegation_vote.tx_id
    JOIN drep_hash ON drep_hash.id = delegation_vote.drep_hash_id
    JOIN stake_address ON stake_address.id = delegation_vote.addr_id
        AND NOT EXISTS (
            SELECT
                *
            FROM
                delegation_vote AS dv2
        WHERE
            dv2.addr_id = delegation_vote.addr_id
            AND dv2.tx_id > delegation_vote.tx_id);
DROP VIEW IF EXISTS current_epoch_params;
CREATE VIEW current_epoch_params AS
SELECT
    ROW_TO_JSON(epoch_param)
FROM
    epoch_param
ORDER BY
    epoch_no DESC
LIMIT 1;
DROP VIEW IF EXISTS network_metrics;
CREATE VIEW network_metrics AS
WITH current_epoch AS (
    SELECT
        Max(NO) AS no
    FROM
        epoch
),
current_block AS (
    SELECT
        Max(block_no) AS block_no
    FROM
        block
),
unique_delegators AS (
    SELECT
        count(DISTINCT (addr_id)) AS count
    FROM
        delegation_vote
),
total_delegations AS (
    SELECT
        count(*) AS count
    FROM
        delegation_vote
),
total_gov_action_proposals AS (
    SELECT
        count(DISTINCT (tx_id, INDEX)) AS count
    FROM
        gov_action_proposal
),
total_drep_votes AS (
    SELECT
        count(*) AS count
    FROM
        voting_procedure
    WHERE
        voter_role = 'DRep'
),
total_registered_dreps AS (
    SELECT
        count(*) AS count
    FROM
        drep_hash
),
always_abstain_voting_power AS (
    SELECT
        coalesce((
            SELECT
                amount
            FROM drep_hash
        LEFT JOIN drep_distr ON drep_hash.id = drep_distr.hash_id
        WHERE
            drep_hash.view = 'drep_always_abstain' ORDER BY epoch_no DESC LIMIT 1), 0) AS amount
),
always_no_confidence_voting_power AS (
    SELECT
        coalesce((
            SELECT
                amount
            FROM drep_hash
        LEFT JOIN drep_distr ON drep_hash.id = drep_distr.hash_id
    WHERE
        drep_hash.view = 'drep_always_no_confidence' ORDER BY epoch_no DESC LIMIT 1), 0) AS amount
)
SELECT
    current_epoch.no AS current_epoch_no,
    current_block.block_no AS current_block_no,
    unique_delegators.count AS unique_delegators_count,
    total_delegations.count AS total_delegators_count,
    total_gov_action_proposals.count AS total_gov_action_proposals_count,
    total_drep_votes.count AS total_drep_votes_count,
    total_registered_dreps.count AS total_registered_dreps_count,
    always_abstain_voting_power.amount AS always_abstain_voting_power_amount,
    always_no_confidence_voting_power.amount AS always_no_confidence_voting_power_amount
FROM
    current_epoch
    CROSS JOIN current_block
    CROSS JOIN unique_delegators
    CROSS JOIN total_delegations
    CROSS JOIN total_gov_action_proposals
    CROSS JOIN total_drep_votes
    CROSS JOIN total_registered_dreps
    CROSS JOIN always_abstain_voting_power
    CROSS JOIN always_no_confidence_voting_power;
DROP VIEW IF EXISTS stake_key_voting_power;
CREATE VIEW stake_key_voting_power AS
SELECT
    coalesce(sum(utxo_view.value), 0) AS voting_power,
    encode(stake_address.hash_raw, 'hex') AS stake_key_hash
FROM
    stake_address
    JOIN utxo_view ON utxo_view.stake_address_id = stake_address.id
GROUP BY
    stake_address.hash_raw;
DROP VIEW IF EXISTS vote;
CREATE VIEW vote AS SELECT DISTINCT ON (voting_procedure.gov_action_proposal_id, voting_procedure.drep_voter)
    voting_procedure.drep_voter AS drep_id,
    voting_procedure.gov_action_proposal_id AS proposal_id,
    concat(encode(gov_action_tx.hash, 'hex'), '#', gov_action_proposal.index) AS proposal_ref,
    encode(drep_hash.raw, 'hex') AS drep_hash,
    voting_procedure.vote::text AS vote,
    voting_anchor.url AS url,
    encode(voting_anchor.data_hash, 'hex') AS vote_metadata_hash,
    block.epoch_no AS epoch_no,
    block.time AS time,
    encode(vote_tx.hash, 'hex') AS vote_tx_hash
FROM
    voting_procedure
    JOIN gov_action_proposal ON gov_action_proposal.id = voting_procedure.gov_action_proposal_id
    JOIN drep_hash ON drep_hash.id = voting_procedure.drep_voter
    LEFT JOIN voting_anchor ON voting_anchor.id = voting_procedure.voting_anchor_id
    JOIN tx AS vote_tx ON vote_tx.id = voting_procedure.tx_id
    JOIN tx AS gov_action_tx ON gov_action_tx.id = gov_action_proposal.tx_id
    JOIN block ON block.id = gov_action_tx.id
ORDER BY
    voting_procedure.gov_action_proposal_id,
    voting_procedure.drep_voter,
    voting_procedure.id DESC;
DROP VIEW IF EXISTS drep_voting_power;
CREATE VIEW drep_voting_power AS
WITH LatestDrepDistr AS (
    SELECT
        hash_id,
        MAX(epoch_no) AS max_epoch_no
    FROM
        drep_distr
    GROUP BY
        hash_id
)
SELECT
    COALESCE(dd.amount, 0) AS amount,
    ENCODE(dh.raw, 'hex') AS drep_hash,
    dh.view AS drep_view
FROM
    drep_hash dh
    LEFT JOIN LatestDrepDistr ldd ON dh.id = ldd.hash_id
    LEFT JOIN drep_distr dd ON ldd.max_epoch_no = dd.epoch_no
        AND dh.id = dd.hash_id;
DROP VIEW IF EXISTS drep;
CREATE VIEW drep AS
WITH DRepDistr AS (
    SELECT
        *,
        ROW_NUMBER() OVER (PARTITION BY drep_hash.id ORDER BY drep_distr.epoch_no DESC) AS rn
    FROM
        drep_distr
        JOIN drep_hash ON drep_hash.id = drep_distr.hash_id
),
DRepActivity AS (
    SELECT
        drep_activity AS drep_activity,
        epoch_no AS epoch_no
    FROM
        epoch_param
    WHERE
        epoch_no IS NOT NULL
    ORDER BY
        epoch_no DESC
    LIMIT 1
)
SELECT DISTINCT ON (dh.raw)
    dh.id AS id,
    encode(dh.raw, 'hex') AS drep_hash,
    dh.view AS drep_view,
    latest_voting_anchor.url AS url,
    encode(latest_voting_anchor.data_hash, 'hex') AS drep_metadata_hash,
    dr_deposit.deposit AS deposit,
    DRepDistr.amount AS drep_voting_power,
    encode(latest_drep_registration.tx_hash, 'hex') AS latest_tx_hash,
    drep_registration.id IS NOT NULL AS was_registered_as_drep,
    solevoter_registration.id IS NOT NULL AS was_registered_as_solevoter,
(
        CASE WHEN latest_drep_registration.deposit >= 0
            AND latest_drep_registration.voting_anchor_id IS NOT NULL THEN
            'DRep'
        WHEN latest_drep_registration.deposit >= 0
            AND latest_drep_registration.voting_anchor_id IS NULL THEN
            'SoleVoter'
        WHEN latest_drep_registration.deposit < 0
            AND second_to_newest_drep_registration.voting_anchor_id IS NOT NULL THEN
            'DRep'
        WHEN latest_drep_registration.deposit < 0
            AND second_to_newest_drep_registration.voting_anchor_id IS NULL THEN
            'SoleVoter'
        ELSE
            'DRep'
        END) AS drep_type,
(
        CASE WHEN dr_deposit.deposit < 0 THEN
            'Retired'
        WHEN ((DRepActivity.epoch_no - greatest(latest_vote_block.epoch_no, block_first_register.epoch_no)) <= DRepActivity.drep_activity)
            AND dr_deposit.deposit >= 0 THEN
            'Active'
        WHEN NOT ((DRepActivity.epoch_no - greatest(latest_vote_block.epoch_no, block_first_register.epoch_no)) <= DRepActivity.drep_activity)
            AND dr_deposit.deposit >= 0 THEN
            'Inactive'
        ELSE
            'Unknown'
        END) AS status,
        newestRegister.time as time
FROM
    drep_hash dh
    JOIN (
        SELECT
            dr.id,
            dr.drep_hash_id,
            dr.deposit,
            ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn
        FROM
            drep_registration dr
        WHERE
            dr.deposit IS NOT NULL) AS dr_deposit ON dr_deposit.drep_hash_id = dh.id
    AND dr_deposit.rn = 1
    LEFT JOIN (
        SELECT
            dr.id,
            dr.drep_hash_id,
            dr.voting_anchor_id,
            ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn
        FROM
            drep_registration dr
        WHERE
            dr.voting_anchor_id IS NOT NULL) AS latest_voting_anchor_registration ON latest_voting_anchor_registration.drep_hash_id = dh.id
    AND latest_voting_anchor_registration.rn = 1
    LEFT JOIN voting_anchor AS latest_voting_anchor ON latest_voting_anchor.id = latest_voting_anchor_registration.voting_anchor_id
    LEFT JOIN (
        SELECT
            dr.id,
            dr.drep_hash_id,
            dr.voting_anchor_id,
            dr.deposit,
            tx.hash AS tx_hash,
            ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn
        FROM
            drep_registration dr
            JOIN tx ON dr.tx_id = tx.id) AS latest_drep_registration ON latest_drep_registration.drep_hash_id = dh.id
        AND latest_drep_registration.rn = 1
    LEFT JOIN (
        SELECT
            dr.id,
            dr.drep_hash_id,
            dr.voting_anchor_id,
            ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id DESC) AS rn
        FROM
            drep_registration dr) AS second_to_newest_drep_registration ON second_to_newest_drep_registration.drep_hash_id = dh.id
        AND second_to_newest_drep_registration.rn = 2
    LEFT JOIN DRepDistr ON DRepDistr.hash_id = dh.id
        AND DRepDistr.rn = 1
    CROSS JOIN DRepActivity
    LEFT JOIN (
        SELECT
            voting_procedure.tx_id,
            drep_hash.id AS drep_hash_id,
            ROW_NUMBER() OVER (PARTITION BY drep_hash.id ORDER BY voting_procedure.tx_id DESC) AS rn
        FROM
            drep_hash
            JOIN voting_procedure ON voting_procedure.drep_voter = drep_hash.id) AS dr_latest_vote ON dr_latest_vote.drep_hash_id = dh.id
        AND dr_latest_vote.rn = 1
    LEFT JOIN tx AS tx ON tx.id = dr_latest_vote.tx_id
    LEFT JOIN block AS latest_vote_block ON latest_vote_block.id = tx.block_id
    JOIN (
        SELECT
            block.time,
            dr.drep_hash_id,
            ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY d.tx_id DESC) AS rn
        FROM
            drep_registration dr
            JOIN tx ON tx.id = dr.tx_id
            JOIN block ON block.id = tx.block_id
        WHERE
            NOT (dr.deposit > 0)) AS newestRegister ON newestRegister.drep_hash_id = dh.id
    AND newestRegister.rn = 1
    JOIN (
        SELECT
            dr.tx_id,
            dr.drep_hash_id,
            ROW_NUMBER() OVER (PARTITION BY dr.drep_hash_id ORDER BY dr.tx_id ASC) AS rn
        FROM
            drep_registration dr) AS dr_first_register ON dr_first_register.drep_hash_id = dh.id
        AND dr_first_register.rn = 1
    JOIN tx AS tx_first_register ON tx_first_register.id = dr_first_register.tx_id
    JOIN block AS block_first_register ON block_first_register.id = tx_first_register.block_id
    LEFT JOIN drep_registration AS drep_registration ON drep_registration.drep_hash_id = dh.id
        AND drep_registration.voting_anchor_id IS NOT NULL
    LEFT JOIN drep_registration AS solevoter_registration ON solevoter_registration.drep_hash_id = dh.id
        AND solevoter_registration.voting_anchor_id IS NULL
GROUP BY
    dh.id,
    dh.raw,
    second_to_newest_drep_registration.voting_anchor_id,
    dh.view,
    dr_deposit.deposit,
    DRepDistr.amount,
    DRepActivity.epoch_no,
    DRepActivity.drep_activity,
    latest_voting_anchor.url,
    latest_voting_anchor.data_hash,
    latest_drep_registration.tx_hash,
    drep_registration.id,
    solevoter_registration.id,
    latest_drep_registration.voting_anchor_id,
    latest_vote_block.epoch_no,
    block_first_register.epoch_no,
    latest_drep_registration.deposit;
DROP VIEW IF EXISTS proposal;
CREATE VIEW proposal AS
WITH LatestDrepDistr AS (
    SELECT
        *,
        ROW_NUMBER() OVER (PARTITION BY hash_id ORDER BY epoch_no DESC) AS rn
    FROM
        drep_distr
),
EpochUtils AS (
    SELECT
        (Max(end_time) - Min(end_time)) /(Max(NO) - Min(NO)) AS epoch_duration,
        Max(NO) AS last_epoch_no,
        Max(end_time) AS last_epoch_end_time
    FROM
        epoch
),
always_no_confidence_voting_power AS (
    SELECT
        coalesce((
            SELECT
                amount
            FROM drep_hash
        LEFT JOIN drep_distr ON drep_hash.id = drep_distr.hash_id
        WHERE
            drep_hash.view = 'drep_always_no_confidence' ORDER BY epoch_no DESC LIMIT 1), 0) AS amount
),
always_abstain_voting_power AS (
    SELECT
        coalesce((
            SELECT
                amount
            FROM drep_hash
        LEFT JOIN drep_distr ON drep_hash.id = drep_distr.hash_id
    WHERE
        drep_hash.view = 'drep_always_abstain' ORDER BY epoch_no DESC LIMIT 1), 0) AS amount
)
SELECT
    gov_action_proposal.id AS id,
    encode(creator_tx.hash, 'hex') AS created_tx_hash,
    gov_action_proposal.index AS created_tx_index,
    gov_action_proposal.type::text AS type,
(
        CASE WHEN gov_action_proposal.type = 'TreasuryWithdrawals' THEN
            json_build_object('Reward Address', stake_address.view, 'Amount', treasury_withdrawal.amount)
        WHEN gov_action_proposal.type::text = 'InfoAction' THEN
            json_build_object()
        ELSE
            NULL
        END) AS description,
    epoch_utils.last_epoch_end_time + epoch_utils.epoch_duration *(gov_action_proposal.expiration - epoch_utils.last_epoch_no) AS expiry_date,
    gov_action_proposal.expiration AS expiry_epoch_no,
    creator_block.time AS created_date,
    creator_block.epoch_no AS created_epoch_no,
    /* created date */
    voting_anchor.url AS metadata_url,
    encode(voting_anchor.data_hash, 'hex') AS metadata_hash,
    off_chain_vote_data.title AS title,
    off_chain_vote_data.abstract AS abstract,
    off_chain_vote_data.motivation AS motivation,
    off_chain_vote_data.rationale AS rationale,
    off_chain_vote_data.json AS metadata_json,
    off_chain_vote_data.json #> '{body, references}' AS reference,
    coalesce(Sum(ldd.amount) FILTER (WHERE voting_procedure.vote::text = 'Yes'), 0) +(
        CASE WHEN gov_action_proposal.type = 'NoConfidence' THEN
            always_no_confidence_voting_power.amount
        ELSE
            0
        END) AS yes_votes,
    coalesce(Sum(ldd.amount) FILTER (WHERE voting_procedure.vote::text = 'No'), 0) +(
        CASE WHEN gov_action_proposal.type = 'NoConfidence' THEN
            0
        ELSE
            always_no_confidence_voting_power.amount
        END) AS no_votes,
    coalesce(Sum(ldd.amount) FILTER (WHERE voting_procedure.vote::text = 'Abstain'), 0) + always_abstain_voting_power.amount AS abstain_votes
FROM
    gov_action_proposal
    LEFT JOIN treasury_withdrawal ON gov_action_proposal.id = treasury_withdrawal.gov_action_proposal_id
    LEFT JOIN stake_address ON stake_address.id = treasury_withdrawal.stake_address_id
    CROSS JOIN EpochUtils AS epoch_utils
    CROSS JOIN always_no_confidence_voting_power
    CROSS JOIN always_abstain_voting_power
    JOIN tx AS creator_tx ON creator_tx.id = gov_action_proposal.tx_id
    JOIN block AS creator_block ON creator_block.id = creator_tx.block_id
    LEFT JOIN voting_anchor ON voting_anchor.id = gov_action_proposal.voting_anchor_id
    LEFT JOIN off_chain_vote_data ON off_chain_vote_data.voting_anchor_id = voting_anchor.id
    LEFT JOIN voting_procedure ON voting_procedure.gov_action_proposal_id = gov_action_proposal.id
    LEFT JOIN LatestDrepDistr ldd ON ldd.hash_id = voting_procedure.drep_voter
        AND ldd.rn = 1
        AND gov_action_proposal.expiration >(
            SELECT
                Max(NO)
            FROM
                epoch)
        AND gov_action_proposal.ratified_epoch IS NULL
        AND gov_action_proposal.enacted_epoch IS NULL
        AND gov_action_proposal.expired_epoch IS NULL
        AND gov_action_proposal.dropped_epoch IS NULL
GROUP BY
    (gov_action_proposal.id,
        stake_address.view,
        treasury_withdrawal.amount,
        creator_block.epoch_no,
        off_chain_vote_data.title,
        off_chain_vote_data.abstract,
        off_chain_vote_data.motivation,
        off_chain_vote_data.rationale,
        off_chain_vote_data.json,
        gov_action_proposal.index,
        creator_tx.hash,
        creator_block.time,
        epoch_utils.epoch_duration,
        epoch_utils.last_epoch_no,
        epoch_utils.last_epoch_end_time,
        voting_anchor.url,
        voting_anchor.data_hash,
        always_no_confidence_voting_power.amount,
        always_abstain_voting_power.amount);
COMMIT;

