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
)

select
    governance_action.id,
    encode(creator_tx.hash, 'hex'),
    governance_action.index,
    governance_action.type::text,
    to_json(governance_action.description::text),
    epoch_utils.last_epoch_end_time + epoch_utils.epoch_duration*(governance_action.expiration - epoch_utils.last_epoch_no),
    creator_block.time, /* created date */
    voting_anchor.url,
    encode(voting_anchor.data_hash, 'hex'),
    coalesce(Sum(ldd.amount) filter (where voting_procedure.vote::text = 'Yes'),0) "yes_votes",
    coalesce((select no_confidence_drep_distr.amount from drep_distr as no_confidence_drep_distr join drep_hash on no_confidence_drep_distr.hash_id = drep_hash.id where drep_hash.view='drep_always_no_confidence' order by no_confidence_drep_distr.epoch_no desc limit 1) + Sum(ldd.amount) filter (where voting_procedure.vote::text = 'No'), 0) "no_votes",
    coalesce((select abstain_drep_distr.amount from drep_distr as abstain_drep_distr join drep_hash on abstain_drep_distr.hash_id = drep_hash.id where drep_hash.view='drep_always_abstain' order by abstain_drep_distr.epoch_no desc limit 1), 0) "abstain_votes"
from governance_action
cross join EpochUtils as epoch_utils
join tx as creator_tx
on creator_tx.id = governance_action.tx_id
join block as creator_block
on creator_block.id = creator_tx.block_id
join voting_anchor
on voting_anchor.id = governance_action.voting_anchor_id
left join voting_procedure
on voting_procedure.governance_action_id = governance_action.id
left join LatestDrepDistr ldd
on ldd.hash_id = voting_procedure.drep_voter and ldd.rn = 1

where (not ? or (concat(encode(creator_tx.hash,'hex'),'#',governance_action.index) in ?))
and governance_action.expiration > (select Max(no) from epoch)
and governance_action.ratified_epoch is null
and governance_action.enacted_epoch is null
and governance_action.expired_epoch is null
and governance_action.dropped_epoch is null

group by (governance_action.id, governance_action.index, creator_tx.hash, creator_block.time, epoch_utils.epoch_duration, epoch_utils.last_epoch_no, epoch_utils.last_epoch_end_time, voting_anchor.url, voting_anchor.data_hash)
