with current_epoch as (
    select Max(no) as no
    from epoch
), current_block as (
    select Max(block_no) as block_no
    from block
), unique_delegators as (
    select count(distinct(addr_id)) as count
    from delegation_vote
), total_delegations as (
    select count(*) as count
    from delegation_vote
), total_gov_action_proposals as (
    select count(distinct(tx_id, index)) as count
    from gov_action_proposal
), total_drep_votes as (
    select count(*) as count
    from voting_procedure
    where voter_role = 'DRep'
), total_registered_dreps as (
    select count(*) as count
    from drep_hash
), always_abstain_voting_power as (
    select amount
    from drep_distr
    join drep_hash
    on drep_hash.id = drep_distr.hash_id
    where drep_hash.view = 'drep_always_abstain'
    order by epoch_no desc
    limit 1
), always_no_confidence_voting_power as (
    select amount
    from drep_distr
    join drep_hash
    on drep_hash.id = drep_distr.hash_id
    where drep_hash.view = 'drep_always_no_confidence'
    order by epoch_no desc
    limit 1
)

select
    current_epoch.no,
    current_block.block_no,
    unique_delegators.count,
    total_delegations.count,
    total_gov_action_proposals.count,
    total_drep_votes.count,
    total_registered_dreps.count,
    always_abstain_voting_power.amount,
    always_no_confidence_voting_power.amount
from current_epoch
cross join current_block
cross join unique_delegators
cross join total_delegations
cross join total_gov_action_proposals
cross join total_drep_votes
cross join total_registered_dreps
cross join always_abstain_voting_power
cross join always_no_confidence_voting_power
