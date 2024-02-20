WITH LatestDrepDistr AS (
    SELECT
        *,
        ROW_NUMBER() OVER(PARTITION BY hash_id ORDER BY epoch_no DESC) AS rn
    FROM drep_distr
), EpochUtils AS (
    SELECT
        (Max(end_time)-Min(end_time)) / (Max(no)-Min(no)) as epoch_duration,
        Max(no) as last_epoch_no,
        Max(end_time) as last_epoch_end_time
    FROM epoch
), always_no_confidence_voting_power as (
    select coalesce(amount, 0) as amount
    from drep_hash
    left join drep_distr
    on drep_hash.id = drep_distr.hash_id
    where drep_hash.view = 'drep_always_no_confidence'
    order by epoch_no desc
    limit 1
), always_abstain_voting_power as (
    select coalesce(amount, 0) as amount
    from drep_hash
    left join drep_distr
    on drep_hash.id = drep_distr.hash_id
    where drep_hash.view = 'drep_always_abstain'
    order by epoch_no desc
    limit 1
)

select
    gov_action_proposal.id,
    encode(creator_tx.hash, 'hex'),
    gov_action_proposal.index,
    gov_action_proposal.type::text,
    gov_action_proposal.description::json,
    epoch_utils.last_epoch_end_time + epoch_utils.epoch_duration*(gov_action_proposal.expiration - epoch_utils.last_epoch_no),
    creator_block.time, /* created date */
    voting_anchor.url,
    encode(voting_anchor.data_hash, 'hex'),

    coalesce(Sum(ldd.amount) filter (where voting_procedure.vote::text = 'Yes'),0)
        + (case
                when gov_action_proposal.type = 'NoConfidence' then always_no_confidence_voting_power.amount
                else 0
            end) "yes_votes",
    coalesce(Sum(ldd.amount) filter (where voting_procedure.vote::text = 'No'),0) +
        (case
                when gov_action_proposal.type = 'NoConfidence' then 0
                else always_no_confidence_voting_power.amount
            end) "no_votes",
    coalesce(Sum(ldd.amount) filter (where voting_procedure.vote::text = 'Abstain'),0) + always_abstain_voting_power.amount "abstain_votes"
from gov_action_proposal
cross join EpochUtils as epoch_utils
cross join always_no_confidence_voting_power
cross join always_abstain_voting_power
join tx as creator_tx
on creator_tx.id = gov_action_proposal.tx_id
join block as creator_block
on creator_block.id = creator_tx.block_id
join voting_anchor
on voting_anchor.id = gov_action_proposal.voting_anchor_id
left join voting_procedure
on voting_procedure.gov_action_proposal_id = gov_action_proposal.id
left join LatestDrepDistr ldd
on ldd.hash_id = voting_procedure.drep_voter and ldd.rn = 1

where (not ? or (concat(encode(creator_tx.hash,'hex'),'#',gov_action_proposal.index) in ?))
and gov_action_proposal.expiration > (select Max(no) from epoch)
and gov_action_proposal.ratified_epoch is null
and gov_action_proposal.enacted_epoch is null
and gov_action_proposal.expired_epoch is null
and gov_action_proposal.dropped_epoch is null

group by (
    gov_action_proposal.id,
    gov_action_proposal.index,
    creator_tx.hash,
    creator_block.time,
    epoch_utils.epoch_duration,
    epoch_utils.last_epoch_no,
    epoch_utils.last_epoch_end_time,
    voting_anchor.url,
    voting_anchor.data_hash,
    always_no_confidence_voting_power.amount,
    always_abstain_voting_power.amount
    )
