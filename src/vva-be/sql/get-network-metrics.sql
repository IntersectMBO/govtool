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
), total_governance_actions as (
    select count(distinct(tx_id, index)) as count
    from governance_action
), total_drep_votes as (
    select count(*) as count
    from voting_procedure
    where voter_role = 'DRep'
), total_registered_dreps as (
    select count(*) as count
    from drep_hash
)

select
    current_epoch.no,
    current_block.block_no,
    unique_delegators.count,
    total_delegations.count,
    total_governance_actions.count,
    total_drep_votes.count,
    total_registered_dreps.count
from current_epoch
cross join current_block
cross join unique_delegators
cross join total_delegations
cross join total_governance_actions
cross join total_drep_votes
cross join total_registered_dreps
