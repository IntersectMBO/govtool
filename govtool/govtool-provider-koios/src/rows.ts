/**
 * The Koios payloads this provider reads (Koios v1.4.x, as served by
 * api.koios.rest). Internal wire shapes, never exported as contract.
 *
 * Nullability is broad: PostgREST renders SQL NULL as JSON null for nearly
 * every non-key column, whatever the published spec says.
 */

export type KoiosVote = 'Yes' | 'No' | 'Abstain';
export type KoiosVoterRole = 'ConstitutionalCommittee' | 'DRep' | 'SPO';

/** Koios keeps db-sync's `NewCommittee` for the ledger's `UpdateCommittee`. */
export type KoiosProposalType =
  | 'ParameterChange'
  | 'HardForkInitiation'
  | 'TreasuryWithdrawals'
  | 'NoConfidence'
  | 'NewCommittee'
  | 'NewConstitution'
  | 'InfoAction';

export interface TipRow {
  hash: string;
  epoch_no: number;
  era?: string | null;
  abs_slot: number;
  epoch_slot: number;
  block_height?: number | null;
  block_no?: number | null;
  block_time: number;
}

/** `/genesis`: the Shelley genesis, every value but `systemstart` as a string. */
export interface GenesisRow {
  networkmagic: string;
  networkid: string;
  epochlength: string;
  slotlength: string;
  activeslotcoeff?: string | number | null;
  updatequorum?: string | number | null;
  maxlovelacesupply?: string | number | null;
  /** UNIX seconds. */
  systemstart?: number | string | null;
  slotsperkesperiod?: string | number | null;
  maxkesrevolutions?: string | number | null;
  securityparam?: string | number | null;
}

export interface EpochInfoRow {
  epoch_no: number;
  start_time: number;
  end_time: number;
  active_stake: string | null;
}

export interface EpochParamsRow {
  epoch_no: number;
  protocol_major: number | null;
  protocol_minor: number | null;
  min_fee_a: number | null;
  min_fee_b: number | null;
  max_tx_size: number | null;
  max_val_size: number | string | null;
  key_deposit: string | null;
  pool_deposit: string | null;
  coins_per_utxo_size: string | null;
  gov_action_lifetime: number | null;
  gov_action_deposit: string | null;
  drep_deposit: string | null;
  drep_activity: number | null;
  committee_min_size: number | null;
  committee_max_term_length: number | null;
  dvt_motion_no_confidence: number | null;
  dvt_committee_normal: number | null;
  dvt_committee_no_confidence: number | null;
  dvt_update_to_constitution: number | null;
  dvt_hard_fork_initiation: number | null;
  dvt_p_p_network_group: number | null;
  dvt_p_p_economic_group: number | null;
  dvt_p_p_technical_group: number | null;
  dvt_p_p_gov_group: number | null;
  dvt_treasury_withdrawal: number | null;
  pvt_motion_no_confidence: number | null;
  pvt_committee_normal: number | null;
  pvt_committee_no_confidence: number | null;
  pvt_hard_fork_initiation: number | null;
  pvtpp_security_group: number | null;
  min_fee_ref_script_cost_per_byte: number | null;
  max_block_size: number | null;
  max_bh_size: number | null;
  max_tx_ex_mem: number | string | null;
  max_tx_ex_steps: number | string | null;
  max_block_ex_mem: number | string | null;
  max_block_ex_steps: number | string | null;
  collateral_percent: number | null;
  max_collateral_inputs: number | null;
  price_mem: number | null;
  price_step: number | null;
  /** Per language, the integer array in ledger parameter order. */
  cost_models: Record<string, unknown> | null;
  max_epoch: number | null;
  optimal_pool_count: number | null;
  influence: number | null;
  monetary_expand_rate: number | null;
  treasury_growth_rate: number | null;
  min_pool_cost: string | number | null;
}

export interface TotalsRow {
  epoch_no: number;
  treasury: string | null;
  reserves: string | null;
}

export interface AccountInfoRow {
  stake_address: string;
  status: 'registered' | 'not registered';
  delegated_drep: string | null;
  delegated_pool: string | null;
  utxo: string;
  rewards: string;
  withdrawals: string;
  rewards_available: string;
  reserves: string;
  treasury: string;
  /** The spec spells it `proposal-refund`; the deployment sends `proposal_refund`. */
  proposal_refund?: string | null;
  'proposal-refund'?: string | null;
}

export interface AccountUpdate {
  action_type: 'registration' | 'deregistration' | 'delegation_pool' | 'delegation_drep' | 'withdrawal';
  tx_hash: string;
  epoch_no: number;
  epoch_slot: number;
  absolute_slot: number;
  block_time: number;
}

export interface AccountUpdatesRow {
  stake_address: string;
  updates: AccountUpdate[] | null;
}

export interface DRepListRow {
  drep_id: string;
  hex: string;
  has_script: boolean;
  /** Served by the deployment (the spec documents `drep_status`). */
  registered?: boolean;
}

export interface DRepInfoRow {
  drep_id: string;
  hex: string | null;
  has_script: boolean;
  drep_status: 'registered' | 'deregistered' | 'not_registered';
  deposit: string | null;
  active: boolean;
  expires_epoch_no: number | null;
  amount: string | null;
  meta_url: string | null;
  meta_hash: string | null;
  live_delegator_count: number | null;
}

export interface DRepUpdateRow {
  drep_id: string;
  update_tx_hash: string;
  cert_index: number;
  block_time: number;
  action: 'registered' | 'updated' | 'deregistered';
  deposit: string | null;
  meta_url: string | null;
  meta_hash: string | null;
}

export interface VotingPowerHistoryRow {
  drep_id?: string;
  pool_id_bech32?: string;
  epoch_no: number;
  amount: string | null;
}

export interface CommitteeMemberEntry {
  status: 'authorized' | 'not_authorized' | 'resigned';
  cc_cold_hex: string;
  cc_cold_has_script: boolean;
  cc_hot_hex: string | null;
  cc_hot_has_script: boolean | null;
  expiration_epoch: number | null;
  cc_cold_id?: string | null;
  cc_hot_id?: string | null;
}

export interface CommitteeInfoRow {
  proposal_id: string | null;
  proposal_tx_hash: string | null;
  proposal_index: number | null;
  quorum_numerator: number | null;
  quorum_denominator: number | null;
  members: CommitteeMemberEntry[] | null;
}

export interface ProposalRow {
  block_time: number;
  proposal_id: string;
  proposal_tx_hash: string;
  proposal_index: number;
  proposal_type: KoiosProposalType;
  proposal_description: unknown;
  previous_gov_action_proposal_id: string | null;
  deposit: string | null;
  return_address: string | null;
  proposed_epoch: number;
  ratified_epoch: number | null;
  enacted_epoch: number | null;
  dropped_epoch: number | null;
  expired_epoch: number | null;
  expiration: number | null;
  meta_url: string | null;
  meta_hash: string | null;
  withdrawal: { stake_address: string; amount: string }[] | null;
}

export interface VotingSummaryRow {
  proposal_type: KoiosProposalType;
  epoch_no: number;
  drep_active_yes_vote_power: string | null;
  drep_yes_vote_power: string | null;
  drep_active_no_vote_power: string | null;
  drep_no_vote_power: string | null;
  drep_active_abstain_vote_power: string | null;
  drep_always_no_confidence_vote_power: string | null;
  drep_always_abstain_vote_power: string | null;
  pool_active_yes_vote_power: string | null;
  pool_yes_vote_power: string | null;
  pool_active_no_vote_power: string | null;
  pool_no_vote_power: string | null;
  pool_active_abstain_vote_power: string | null;
  pool_passive_always_abstain_vote_power: string | null;
  pool_passive_always_no_confidence_vote_power: string | null;
  committee_yes_votes_cast: number | null;
  committee_no_votes_cast: number | null;
  committee_abstain_votes_cast: number | null;
}

export interface VoteListRow {
  vote_tx_hash: string;
  voter_role: KoiosVoterRole;
  voter_id: string;
  proposal_id: string;
  proposal_tx_hash: string;
  proposal_index: number;
  proposal_type: KoiosProposalType;
  epoch_no: number;
  block_height: number | null;
  block_time: number;
  vote: KoiosVote;
  meta_url: string | null;
  meta_hash: string | null;
}

export interface PoolListRow {
  pool_id_bech32: string;
  pool_id_hex: string;
  pledge: string | null;
  meta_url: string | null;
  meta_hash: string | null;
  pool_status: 'registered' | 'retiring' | 'retired';
  active_stake: string | null;
}

export interface PoolInfoRow extends PoolListRow {
  live_stake: string | null;
  voting_power: string | null;
}

export interface TxInfoRow {
  tx_hash: string;
  block_height: number | null;
  epoch_no: number;
  absolute_slot: number;
  tx_timestamp: number;
}
