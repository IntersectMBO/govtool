create sequence schema_version_id_seq
    as integer;

create domain lovelace as numeric(20, 0)
    constraint lovelace_check check ((VALUE >= (0)::numeric) AND (VALUE <= '18446744073709551615'::numeric));

create domain txindex as smallint
    constraint txindex_check check (VALUE >= 0);

create domain word31type as integer
    constraint word31type_check check (VALUE >= 0);

create domain hash32type as bytea
    constraint hash32type_check check (octet_length(VALUE) = 32);

create domain hash28type as bytea
    constraint hash28type_check check (octet_length(VALUE) = 28);

create domain addr29type as bytea
    constraint addr29type_check check (octet_length(VALUE) = 29);

create domain word128type as numeric(39, 0)
    constraint word128type_check check ((VALUE >= (0)::numeric) AND
                                        (VALUE <= '340282366920938463463374607431768211455'::numeric));

create domain word64type as numeric(20, 0)
    constraint word64type_check check ((VALUE >= (0)::numeric) AND (VALUE <= '18446744073709551615'::numeric));

create domain outsum as word128type;

create domain asset32type as bytea
    constraint asset32type_check check (octet_length(VALUE) <= 32);

create domain int65type as numeric(20, 0)
    constraint int65type_check check ((VALUE >= '-18446744073709551615'::numeric) AND
                                      (VALUE <= '18446744073709551615'::numeric));

create type rewardtype as enum ('leader', 'member', 'reserves', 'treasury', 'refund');

create type syncstatetype as enum ('lagging', 'following');

create domain word63type as bigint
    constraint word63type_check check (VALUE >= 0);

create type scriptpurposetype as enum ('spend', 'mint', 'cert', 'reward');

create type scripttype as enum ('multisig', 'timelock', 'plutusV1', 'plutusV2');

create table schema_version
(
    id          bigint default nextval('schema_version_id_seq'::regclass) not null
        primary key,
    stage_one   bigint                                                    not null,
    stage_two   bigint                                                    not null,
    stage_three bigint                                                    not null
);


alter sequence schema_version_id_seq owned by schema_version.id;

create table pool_hash
(
    id       bigserial
        primary key,
    hash_raw hash28type not null
        constraint unique_pool_hash
            unique,
    view     varchar    not null
);


create table slot_leader
(
    id           bigserial
        primary key,
    hash         hash28type not null
        constraint unique_slot_leader
            unique,
    pool_hash_id bigint,
    description  varchar    not null
);


create index idx_slot_leader_pool_hash_id
    on slot_leader (pool_hash_id);

create table block
(
    id              bigserial
        primary key,
    hash            hash32type not null
        constraint unique_block
            unique,
    epoch_no        word31type,
    slot_no         word63type,
    epoch_slot_no   word31type,
    block_no        word31type,
    previous_id     bigint,
    slot_leader_id  bigint     not null,
    size            word31type not null,
    time            timestamp  not null,
    tx_count        bigint     not null,
    proto_major     word31type not null,
    proto_minor     word31type not null,
    vrf_key         varchar,
    op_cert         hash32type,
    op_cert_counter word63type
);


create index idx_block_slot_no
    on block (slot_no);

create index idx_block_block_no
    on block (block_no);

create index idx_block_epoch_no
    on block (epoch_no);

create index idx_block_previous_id
    on block (previous_id);

create index idx_block_time
    on block (time);

create index idx_block_slot_leader_id
    on block (slot_leader_id);

create table tx
(
    id                bigserial
        primary key,
    hash              hash32type not null
        constraint unique_tx
            unique,
    block_id          bigint     not null,
    block_index       word31type not null,
    out_sum           lovelace   not null,
    fee               lovelace   not null,
    deposit           bigint     not null,
    size              word31type not null,
    invalid_before    word64type,
    invalid_hereafter word64type,
    valid_contract    boolean    not null,
    script_size       word31type not null
);


create index idx_tx_block_id
    on tx (block_id);

create table stake_address
(
    id          bigserial
        primary key,
    hash_raw    addr29type not null
        constraint unique_stake_address
            unique,
    view        varchar    not null,
    script_hash hash28type
);


create index idx_stake_address_hash_raw
    on stake_address (hash_raw);

create index idx_stake_address_view
    on stake_address using hash (view);

create table tx_out
(
    id                  bigserial
        primary key,
    tx_id               bigint   not null,
    index               txindex  not null,
    address             varchar  not null,
    address_raw         bytea    not null,
    address_has_script  boolean  not null,
    payment_cred        hash28type,
    stake_address_id    bigint,
    value               lovelace not null,
    data_hash           hash32type,
    inline_datum_id     bigint,
    reference_script_id bigint,
    constraint unique_txout
        unique (tx_id, index)
);


create index idx_tx_out_address
    on tx_out using hash (address);

create index idx_tx_out_payment_cred
    on tx_out (payment_cred);

create index idx_tx_out_tx_id
    on tx_out (tx_id);

create index idx_tx_out_stake_address_id
    on tx_out (stake_address_id);

create index tx_out_inline_datum_id_idx
    on tx_out (inline_datum_id);

create index tx_out_reference_script_id_idx
    on tx_out (reference_script_id);

create table datum
(
    id    bigserial
        primary key,
    hash  hash32type not null
        constraint unique_datum
            unique,
    tx_id bigint     not null,
    value jsonb,
    bytes bytea      not null
);


create index idx_datum_tx_id
    on datum (tx_id);

create table redeemer
(
    id               bigserial
        primary key,
    tx_id            bigint            not null,
    unit_mem         word63type        not null,
    unit_steps       word63type        not null,
    fee              lovelace,
    purpose          scriptpurposetype not null,
    index            word31type        not null,
    script_hash      hash28type,
    redeemer_data_id bigint            not null
);


create index redeemer_redeemer_data_id_idx
    on redeemer (redeemer_data_id);

create table tx_in
(
    id           bigserial
        primary key,
    tx_in_id     bigint  not null,
    tx_out_id    bigint  not null,
    tx_out_index txindex not null,
    redeemer_id  bigint
);


create index idx_tx_in_source_tx
    on tx_in (tx_in_id);

create index idx_tx_in_tx_in_id
    on tx_in (tx_in_id);

create index idx_tx_in_tx_out_id
    on tx_in (tx_out_id);

create index idx_tx_in_redeemer_id
    on tx_in (redeemer_id);

create table collateral_tx_in
(
    id           bigserial
        primary key,
    tx_in_id     bigint  not null,
    tx_out_id    bigint  not null,
    tx_out_index txindex not null
);


create index idx_collateral_tx_in_tx_out_id
    on collateral_tx_in (tx_out_id);

create table meta
(
    id           bigserial
        primary key,
    start_time   timestamp not null
        constraint unique_meta
            unique,
    network_name varchar   not null,
    version      varchar   not null
);


create table epoch
(
    id         bigserial
        primary key,
    out_sum    word128type not null,
    fees       lovelace    not null,
    tx_count   word31type  not null,
    blk_count  word31type  not null,
    no         word31type  not null
        constraint unique_epoch
            unique,
    start_time timestamp   not null,
    end_time   timestamp   not null
);


create index idx_epoch_no
    on epoch (no);

create table ada_pots
(
    id       bigserial
        primary key,
    slot_no  word63type not null,
    epoch_no word31type not null,
    treasury lovelace   not null,
    reserves lovelace   not null,
    rewards  lovelace   not null,
    utxo     lovelace   not null,
    deposits lovelace   not null,
    fees     lovelace   not null,
    block_id bigint     not null
);


create table pool_metadata_ref
(
    id               bigserial
        primary key,
    pool_id          bigint     not null,
    url              varchar    not null,
    hash             hash32type not null,
    registered_tx_id bigint     not null,
    constraint unique_pool_metadata_ref
        unique (pool_id, url, hash)
);


create index idx_pool_metadata_ref_registered_tx_id
    on pool_metadata_ref (registered_tx_id);

create index idx_pool_metadata_ref_pool_id
    on pool_metadata_ref (pool_id);

create table pool_update
(
    id               bigserial
        primary key,
    hash_id          bigint           not null,
    cert_index       integer          not null,
    vrf_key_hash     hash32type       not null,
    pledge           lovelace         not null,
    active_epoch_no  bigint           not null,
    meta_id          bigint,
    margin           double precision not null,
    fixed_cost       lovelace         not null,
    registered_tx_id bigint           not null,
    reward_addr_id   bigint           not null
);


create index idx_pool_update_hash_id
    on pool_update (hash_id);

create index idx_pool_update_registered_tx_id
    on pool_update (registered_tx_id);

create index idx_pool_update_meta_id
    on pool_update (meta_id);

create index idx_pool_update_reward_addr
    on pool_update (reward_addr_id);

create index idx_pool_update_active_epoch_no
    on pool_update (active_epoch_no);

create table pool_owner
(
    id             bigserial
        primary key,
    addr_id        bigint not null,
    pool_update_id bigint not null
);


create index pool_owner_pool_update_id_idx
    on pool_owner (pool_update_id);

create table pool_retire
(
    id              bigserial
        primary key,
    hash_id         bigint     not null,
    cert_index      integer    not null,
    announced_tx_id bigint     not null,
    retiring_epoch  word31type not null
);


create index idx_pool_retire_hash_id
    on pool_retire (hash_id);

create index idx_pool_retire_announced_tx_id
    on pool_retire (announced_tx_id);

create table pool_relay
(
    id           bigserial
        primary key,
    update_id    bigint not null,
    ipv4         varchar,
    ipv6         varchar,
    dns_name     varchar,
    dns_srv_name varchar,
    port         integer
);


create index idx_pool_relay_update_id
    on pool_relay (update_id);

create table stake_registration
(
    id         bigserial
        primary key,
    addr_id    bigint     not null,
    cert_index integer    not null,
    epoch_no   word31type not null,
    tx_id      bigint     not null
);


create index idx_stake_registration_tx_id
    on stake_registration (tx_id);

create index idx_stake_registration_addr_id
    on stake_registration (addr_id);

create table stake_deregistration
(
    id          bigserial
        primary key,
    addr_id     bigint     not null,
    cert_index  integer    not null,
    epoch_no    word31type not null,
    tx_id       bigint     not null,
    redeemer_id bigint
);


create index idx_stake_deregistration_tx_id
    on stake_deregistration (tx_id);

create index idx_stake_deregistration_addr_id
    on stake_deregistration (addr_id);

create index idx_stake_deregistration_redeemer_id
    on stake_deregistration (redeemer_id);

create table delegation
(
    id              bigserial
        primary key,
    addr_id         bigint     not null,
    cert_index      integer    not null,
    pool_hash_id    bigint     not null,
    active_epoch_no bigint     not null,
    tx_id           bigint     not null,
    slot_no         word63type not null,
    redeemer_id     bigint
);


create index idx_delegation_pool_hash_id
    on delegation (pool_hash_id);

create index idx_delegation_tx_id
    on delegation (tx_id);

create index idx_delegation_addr_id
    on delegation (addr_id);

create index idx_delegation_active_epoch_no
    on delegation (active_epoch_no);

create index idx_delegation_redeemer_id
    on delegation (redeemer_id);

create table tx_metadata
(
    id    bigserial
        primary key,
    key   word64type not null,
    json  jsonb,
    bytes bytea      not null,
    tx_id bigint     not null
);


create index idx_tx_metadata_tx_id
    on tx_metadata (tx_id);

create table reward
(
    id              bigserial
        primary key,
    addr_id         bigint     not null,
    type            rewardtype not null,
    amount          lovelace   not null,
    earned_epoch    bigint     not null,
    spendable_epoch bigint     not null,
    pool_id         bigint,
    constraint unique_reward
        unique (addr_id, type, earned_epoch, pool_id)
);


create index idx_reward_pool_id
    on reward (pool_id);

create index idx_reward_earned_epoch
    on reward (earned_epoch);

create index idx_reward_addr_id
    on reward (addr_id);

create index idx_reward_spendable_epoch
    on reward (spendable_epoch);

create table withdrawal
(
    id          bigserial
        primary key,
    addr_id     bigint   not null,
    amount      lovelace not null,
    redeemer_id bigint,
    tx_id       bigint   not null
);


create index idx_withdrawal_tx_id
    on withdrawal (tx_id);

create index idx_withdrawal_addr_id
    on withdrawal (addr_id);

create index idx_withdrawal_redeemer_id
    on withdrawal (redeemer_id);

create table epoch_stake
(
    id       bigserial
        primary key,
    addr_id  bigint     not null,
    pool_id  bigint     not null,
    amount   lovelace   not null,
    epoch_no word31type not null,
    constraint unique_stake
        unique (epoch_no, addr_id, pool_id)
);


create index idx_epoch_stake_pool_id
    on epoch_stake (pool_id);

create index idx_epoch_stake_epoch_no
    on epoch_stake (epoch_no);

create index idx_epoch_stake_addr_id
    on epoch_stake (addr_id);

create table treasury
(
    id         bigserial
        primary key,
    addr_id    bigint    not null,
    cert_index integer   not null,
    amount     int65type not null,
    tx_id      bigint    not null
);


create index idx_treasury_tx_id
    on treasury (tx_id);

create index idx_treasury_addr_id
    on treasury (addr_id);

create table reserve
(
    id         bigserial
        primary key,
    addr_id    bigint    not null,
    cert_index integer   not null,
    amount     int65type not null,
    tx_id      bigint    not null
);


create index idx_reserve_tx_id
    on reserve (tx_id);

create index idx_reserve_addr_id
    on reserve (addr_id);

create table pot_transfer
(
    id         bigserial
        primary key,
    cert_index integer   not null,
    treasury   int65type not null,
    reserves   int65type not null,
    tx_id      bigint    not null
);


create table epoch_sync_time
(
    id      bigserial
        primary key,
    no      bigint        not null
        constraint unique_epoch_sync_time
            unique,
    seconds word63type    not null,
    state   syncstatetype not null
);


create table ma_tx_mint
(
    id       bigserial
        primary key,
    quantity int65type not null,
    tx_id    bigint    not null,
    ident    bigint    not null
);


create index idx_ma_tx_mint_tx_id
    on ma_tx_mint (tx_id);

create table ma_tx_out
(
    id        bigserial
        primary key,
    quantity  word64type not null,
    tx_out_id bigint     not null,
    ident     bigint     not null
);


create index idx_ma_tx_out_tx_out_id
    on ma_tx_out (tx_out_id);

create table script
(
    id              bigserial
        primary key,
    tx_id           bigint     not null,
    hash            hash28type not null
        constraint unique_script
            unique,
    type            scripttype not null,
    json            jsonb,
    bytes           bytea,
    serialised_size word31type
);


create index idx_script_tx_id
    on script (tx_id);

create table cost_model
(
    id    bigserial
        primary key,
    costs jsonb      not null,
    hash  hash32type not null
        constraint unique_cost_model
            unique
);


create table param_proposal
(
    id                    bigserial
        primary key,
    epoch_no              word31type not null,
    key                   hash28type not null,
    min_fee_a             word64type,
    min_fee_b             word64type,
    max_block_size        word64type,
    max_tx_size           word64type,
    max_bh_size           word64type,
    key_deposit           lovelace,
    pool_deposit          lovelace,
    max_epoch             word64type,
    optimal_pool_count    word64type,
    influence             double precision,
    monetary_expand_rate  double precision,
    treasury_growth_rate  double precision,
    decentralisation      double precision,
    entropy               hash32type,
    protocol_major        word31type,
    protocol_minor        word31type,
    min_utxo_value        lovelace,
    min_pool_cost         lovelace,
    cost_model_id         bigint,
    price_mem             double precision,
    price_step            double precision,
    max_tx_ex_mem         word64type,
    max_tx_ex_steps       word64type,
    max_block_ex_mem      word64type,
    max_block_ex_steps    word64type,
    max_val_size          word64type,
    collateral_percent    word31type,
    max_collateral_inputs word31type,
    registered_tx_id      bigint     not null,
    coins_per_utxo_size   lovelace
);


create index idx_param_proposal_registered_tx_id
    on param_proposal (registered_tx_id);

create index idx_param_proposal_cost_model_id
    on param_proposal (cost_model_id);

create table epoch_param
(
    id                    bigserial
        primary key,
    epoch_no              word31type       not null,
    min_fee_a             word31type       not null,
    min_fee_b             word31type       not null,
    max_block_size        word31type       not null,
    max_tx_size           word31type       not null,
    max_bh_size           word31type       not null,
    key_deposit           lovelace         not null,
    pool_deposit          lovelace         not null,
    max_epoch             word31type       not null,
    optimal_pool_count    word31type       not null,
    influence             double precision not null,
    monetary_expand_rate  double precision not null,
    treasury_growth_rate  double precision not null,
    decentralisation      double precision not null,
    protocol_major        word31type       not null,
    protocol_minor        word31type       not null,
    min_utxo_value        lovelace         not null,
    min_pool_cost         lovelace         not null,
    nonce                 hash32type,
    cost_model_id         bigint,
    price_mem             double precision,
    price_step            double precision,
    max_tx_ex_mem         word64type,
    max_tx_ex_steps       word64type,
    max_block_ex_mem      word64type,
    max_block_ex_steps    word64type,
    max_val_size          word64type,
    collateral_percent    word31type,
    max_collateral_inputs word31type,
    block_id              bigint           not null,
    extra_entropy         hash32type,
    coins_per_utxo_size   lovelace
);


create index idx_epoch_param_block_id
    on epoch_param (block_id);

create index idx_epoch_param_cost_model_id
    on epoch_param (cost_model_id);

create table pool_offline_data
(
    id          bigserial
        primary key,
    pool_id     bigint     not null,
    ticker_name varchar    not null,
    hash        hash32type not null,
    json        jsonb      not null,
    bytes       bytea      not null,
    pmr_id      bigint     not null,
    constraint unique_pool_offline_data
        unique (pool_id, hash)
);


create index idx_pool_offline_data_pmr_id
    on pool_offline_data (pmr_id);

create table pool_offline_fetch_error
(
    id          bigserial
        primary key,
    pool_id     bigint     not null,
    fetch_time  timestamp  not null,
    pmr_id      bigint     not null,
    fetch_error varchar    not null,
    retry_count word31type not null,
    constraint unique_pool_offline_fetch_error
        unique (pool_id, fetch_time, retry_count)
);


create index idx_pool_offline_fetch_error_pmr_id
    on pool_offline_fetch_error (pmr_id);

create table reserved_pool_ticker
(
    id        bigserial
        primary key,
    name      varchar    not null
        constraint unique_reserved_pool_ticker
            unique,
    pool_hash hash28type not null
);


create index idx_reserved_pool_ticker_pool_hash
    on reserved_pool_ticker (pool_hash);

create table multi_asset
(
    id          bigserial
        primary key,
    policy      hash28type  not null,
    name        asset32type not null,
    fingerprint varchar     not null,
    constraint unique_multi_asset
        unique (policy, name)
);


create table delisted_pool
(
    id       bigserial
        primary key,
    hash_raw hash28type not null
        constraint unique_delisted_pool
            unique
);


create table extra_key_witness
(
    id    bigserial
        primary key,
    hash  hash28type not null,
    tx_id bigint     not null
);


create index idx_extra_key_witness_tx_id
    on extra_key_witness (tx_id);

create table collateral_tx_out
(
    id                  bigserial
        primary key,
    tx_id               bigint   not null,
    index               txindex  not null,
    address             varchar  not null,
    address_raw         bytea    not null,
    address_has_script  boolean  not null,
    payment_cred        hash28type,
    stake_address_id    bigint,
    value               lovelace not null,
    data_hash           hash32type,
    multi_assets_descr  varchar  not null,
    inline_datum_id     bigint,
    reference_script_id bigint
);


create index collateral_tx_out_stake_address_id_idx
    on collateral_tx_out (stake_address_id);

create index collateral_tx_out_inline_datum_id_idx
    on collateral_tx_out (inline_datum_id);

create index collateral_tx_out_reference_script_id_idx
    on collateral_tx_out (reference_script_id);

create table reference_tx_in
(
    id           bigserial
        primary key,
    tx_in_id     bigint  not null,
    tx_out_id    bigint  not null,
    tx_out_index txindex not null
);


create index reference_tx_in_tx_out_id_idx
    on reference_tx_in (tx_out_id);

create table redeemer_data
(
    id    bigserial
        primary key,
    hash  hash32type not null
        constraint unique_redeemer_data
            unique,
    tx_id bigint     not null,
    value jsonb,
    bytes bytea      not null
);


create index redeemer_data_tx_id_idx
    on redeemer_data (tx_id);

create table tx_confirmed
(
    tx_hash           bytea not null
        primary key,
    block_hash        bytea,
    slot_no           bigint,
    block_no          bigint,
    pool_id           varchar(100),
    confirmation_time timestamp
);


create table reverse_index
(
    id       bigserial
        primary key,
    block_id bigint not null,
    min_ids  varchar
);


create view tx_log(tx_hash, block_hash, slot_no, pool_id, block_no, confirmation_time) as
SELECT tx.hash  AS tx_hash,
       b.hash   AS block_hash,
       b.slot_no,
       ph.view  AS pool_id,
       b.block_no,
       b."time" AS confirmation_time
FROM tx
         JOIN block b ON b.id = tx.block_id
         JOIN slot_leader sl ON b.slot_leader_id = sl.id
         JOIN pool_hash ph ON sl.pool_hash_id = ph.id;


create view utxo_byron_view
            (id, tx_id, index, address, address_raw, address_has_script, payment_cred, stake_address_id, value,
             data_hash, inline_datum_id, reference_script_id)
as
SELECT tx_out.id,
       tx_out.tx_id,
       tx_out.index,
       tx_out.address,
       tx_out.address_raw,
       tx_out.address_has_script,
       tx_out.payment_cred,
       tx_out.stake_address_id,
       tx_out.value,
       tx_out.data_hash,
       tx_out.inline_datum_id,
       tx_out.reference_script_id
FROM tx_out
         LEFT JOIN tx_in ON tx_out.tx_id = tx_in.tx_out_id AND tx_out.index::smallint = tx_in.tx_out_index::smallint
WHERE tx_in.tx_in_id IS NULL;


create view utxo_view
            (id, tx_id, index, address, address_raw, address_has_script, payment_cred, stake_address_id, value,
             data_hash, inline_datum_id, reference_script_id)
as
SELECT tx_out.id,
       tx_out.tx_id,
       tx_out.index,
       tx_out.address,
       tx_out.address_raw,
       tx_out.address_has_script,
       tx_out.payment_cred,
       tx_out.stake_address_id,
       tx_out.value,
       tx_out.data_hash,
       tx_out.inline_datum_id,
       tx_out.reference_script_id
FROM tx_out
         LEFT JOIN tx_in ON tx_out.tx_id = tx_in.tx_out_id AND tx_out.index::smallint = tx_in.tx_out_index::smallint
         LEFT JOIN tx ON tx.id = tx_out.tx_id
         LEFT JOIN block ON tx.block_id = block.id
WHERE tx_in.tx_in_id IS NULL
  AND block.epoch_no IS NOT NULL;

