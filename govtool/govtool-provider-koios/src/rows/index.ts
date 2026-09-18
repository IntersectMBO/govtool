/**
 * The Koios payloads this provider reads, as TypeScript.
 *
 * Two rules, both learned by diffing the published `koiosapi.yaml` (v1.4.2)
 * against a live mainnet instance:
 *
 * 1. **The spec and the deployment disagree.** `/drep_list` documents a
 *    `drep_status` enum but serves a `registered` boolean; `/committee_info`
 *    documents only hex credentials but serves the CIP-129 bech32 ids too;
 *    `/account_info` documents `proposal-refund` and serves
 *    `proposal_refund`. Every such field is typed as optional here and read
 *    defensively, because either shape can arrive depending on which release
 *    an operator is running.
 * 2. **Nullability is broad.** PostgREST returns SQL `NULL` as JSON `null`
 *    for most computed columns, so anything that is not a primary key is
 *    nullable whatever the spec claims.
 *
 * Nothing in this file is exported from the package's public surface as a
 * contract: these are the provider's internal wire shapes.
 */

export type KoiosVote = 'Yes' | 'No' | 'Abstain';
export type KoiosVoterRole = 'ConstitutionalCommittee' | 'DRep' | 'SPO';
export type KoiosDRepStatus = 'registered' | 'deregistered' | 'not_registered';

/** Koios keeps the pre-ratification name `NewCommittee` for `UpdateCommittee`. */
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
  /** Newer deployments send both; older ones only `block_height`. */
  block_no?: number | null;
  block_time: number;
}

export interface GenesisRow {
  networkmagic: string;
  networkid: string;
  epochlength: string;
  slotlength: string;
  systemstart: number;
  maxlovelacesupply: string;
}

export interface EpochInfoRow {
  epoch_no: number;
  era?: string | null;
  out_sum: string;
  fees: string;
  tx_count: number;
  blk_count: number;
  start_time: number;
  end_time: number;
  first_block_time: number | null;
  last_block_time: number | null;
  active_stake: string | null;
  total_rewards: string | null;
  avg_blk_reward: string | null;
}

export interface BlockRow {
  hash: string;
  epoch_no: number;
  abs_slot: number;
  epoch_slot: number;
  block_height: number | null;
  block_size: number;
  block_time: number;
  tx_count: number;
}

export interface TotalsRow {
  epoch_no: number;
  circulation: string;
  treasury: string;
  reward: string;
  supply: string;
  reserves: string;
  fees: string;
  deposits_stake?: string | null;
  deposits_drep?: string | null;
  deposits_proposal?: string | null;
}

/** `/epoch_params` — every governance field, plus the rest, passed through as `raw`. */
export interface EpochParamsRow {
  epoch_no: number;
  min_fee_a: number | null;
  min_fee_b: number | null;
  key_deposit: string | null;
  pool_deposit: string | null;
  coins_per_utxo_size: string | null;
  min_fee_ref_script_cost_per_byte: number | null;
  protocol_major: number | null;
  protocol_minor: number | null;
  gov_action_deposit: string | null;
  drep_deposit: string | null;
  drep_activity: number | null;
  gov_action_lifetime: number | null;
  committee_min_size: number | null;
  committee_max_term_length: number | null;
  /** Thresholds are IEEE-754 doubles here; see `network.mapper`. */
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
  [key: string]: unknown;
}

/* ------------------------------------------------------------------------- */
/* Accounts                                                                   */
/* ------------------------------------------------------------------------- */

export interface AccountInfoRow {
  stake_address: string;
  status: 'registered' | 'not registered';
  delegated_pool: string | null;
  delegated_drep: string | null;
  total_balance: string;
  utxo: string;
  rewards: string;
  withdrawals: string;
  rewards_available: string;
  deposit: string;
  reserves: string;
  treasury: string;
  /** Spec spells it `proposal-refund`; the deployment sends `proposal_refund`. */
  proposal_refund?: string | null;
  'proposal-refund'?: string | null;
}

export type AccountUpdateAction =
  | 'registration'
  | 'deregistration'
  | 'delegation_pool'
  | 'delegation_drep'
  | 'withdrawal';

export interface AccountUpdateEntry {
  action_type: AccountUpdateAction;
  tx_hash: string;
  epoch_no: number;
  epoch_slot: number;
  absolute_slot: number;
  block_time: number;
}

export interface AccountUpdatesRow {
  stake_address: string;
  updates: AccountUpdateEntry[];
}

/* ------------------------------------------------------------------------- */
/* DReps                                                                      */
/* ------------------------------------------------------------------------- */

export interface DRepListRow {
  drep_id: string;
  hex: string | null;
  has_script: boolean;
  /** Served by the deployment. */
  registered?: boolean;
  /** Documented by the spec. */
  drep_status?: KoiosDRepStatus;
}

export interface DRepInfoRow {
  drep_id: string;
  hex: string | null;
  has_script: boolean;
  drep_status?: KoiosDRepStatus;
  registered?: boolean;
  deposit: string | null;
  active: boolean;
  expires_epoch_no: number | null;
  amount: string | null;
  meta_url: string | null;
  meta_hash: string | null;
  live_delegator_count: number | null;
}

export interface DRepMetadataRow {
  drep_id: string;
  hex: string | null;
  has_script: boolean;
  meta_url: string | null;
  meta_hash: string | null;
  meta_json: unknown;
  bytes: string | null;
  warning: string | null;
  language: string | null;
  comment: string | null;
  is_valid: boolean | null;
}

export interface DRepUpdateRow {
  drep_id: string;
  hex: string | null;
  has_script: boolean;
  update_tx_hash: string;
  cert_index: number;
  block_time: number;
  action: 'registered' | 'updated' | 'deregistered';
  deposit: string | null;
  meta_url: string | null;
  meta_hash: string | null;
  meta_json: unknown;
}

export interface DRepPowerHistoryRow {
  drep_id: string;
  epoch_no: number;
  amount: string | null;
}

export interface DRepVoteRow {
  proposal_id: string;
  proposal_tx_hash: string;
  proposal_index: number;
  vote_tx_hash: string;
  block_time: number;
  vote: KoiosVote;
  meta_url: string | null;
  meta_hash: string | null;
}

export interface DRepDelegatorRow {
  stake_address: string;
  stake_address_hex: string;
  script_hash: string | null;
  epoch_no: number | null;
  amount: string;
}

export interface DRepEpochSummaryRow {
  epoch_no: number;
  amount: string;
  dreps: number;
}

/* ------------------------------------------------------------------------- */
/* Committee                                                                  */
/* ------------------------------------------------------------------------- */

export interface CommitteeMemberEntry {
  status: 'authorized' | 'not_authorized' | 'resigned';
  cc_cold_hex: string;
  cc_cold_has_script: boolean;
  cc_hot_hex: string | null;
  cc_hot_has_script: boolean | null;
  expiration_epoch: number | null;
  /** Served by the deployment, absent from the spec. */
  cc_cold_id?: string | null;
  cc_hot_id?: string | null;
}

export interface CommitteeInfoRow {
  proposal_id: string | null;
  proposal_tx_hash: string | null;
  proposal_index: number | null;
  quorum_numerator: number;
  quorum_denominator: number;
  members: CommitteeMemberEntry[];
}

export interface CommitteeVoteRow {
  proposal_id: string;
  proposal_tx_hash: string;
  proposal_index: number;
  vote_tx_hash: string;
  block_time: number;
  vote: KoiosVote;
  meta_url: string | null;
  meta_hash: string | null;
}

/* ------------------------------------------------------------------------- */
/* Proposals & votes                                                          */
/* ------------------------------------------------------------------------- */

export interface ProposalWithdrawal {
  stake_address: string;
  amount: string;
}

export interface ProposalRow {
  block_time: number;
  proposal_id: string;
  proposal_tx_hash: string;
  proposal_index: number;
  proposal_type: KoiosProposalType;
  /** The ledger's own JSON rendering of the action body; see `proposal.mapper`. */
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
  meta_json: unknown;
  meta_comment: string | null;
  meta_language: string | null;
  meta_is_valid: boolean | null;
  withdrawal: ProposalWithdrawal[] | null;
  param_proposal: Record<string, unknown> | null;
}

export interface ProposalVotingSummaryRow {
  proposal_type: KoiosProposalType;
  epoch_no: number;
  drep_yes_votes_cast: number;
  drep_active_yes_vote_power: string | null;
  drep_yes_vote_power: string | null;
  drep_yes_pct: number | null;
  drep_no_votes_cast: number;
  drep_active_no_vote_power: string | null;
  drep_no_vote_power: string | null;
  drep_no_pct: number | null;
  drep_abstain_votes_cast: number;
  drep_active_abstain_vote_power: string | null;
  drep_always_no_confidence_vote_power: string | null;
  drep_always_abstain_vote_power: string | null;
  pool_yes_votes_cast: number;
  pool_active_yes_vote_power: string | null;
  pool_yes_vote_power: string | null;
  pool_yes_pct: number | null;
  pool_no_votes_cast: number;
  pool_active_no_vote_power: string | null;
  pool_no_vote_power: string | null;
  pool_no_pct: number | null;
  pool_abstain_votes_cast: number;
  pool_active_abstain_vote_power: string | null;
  committee_yes_votes_cast: number;
  committee_yes_pct: number | null;
  committee_no_votes_cast: number;
  committee_no_pct: number | null;
  committee_abstain_votes_cast: number;
}

/** `/proposal_votes` — note the absent `vote_tx_hash`; see `votes.api`. */
export interface ProposalVoteRow {
  block_time: number;
  voter_role: KoiosVoterRole;
  voter_id: string;
  voter_hex: string;
  voter_has_script: boolean;
  vote: KoiosVote;
  meta_url: string | null;
  meta_hash: string | null;
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
  meta_json: unknown;
}

/* ------------------------------------------------------------------------- */
/* Pools                                                                      */
/* ------------------------------------------------------------------------- */

export interface PoolListRow {
  pool_id_bech32: string;
  pool_id_hex: string;
  ticker: string | null;
  pledge: string | null;
  active_stake: string | null;
  pool_status: 'registered' | 'retiring' | 'retired';
}

export interface PoolInfoRow extends PoolListRow {
  meta_json: { name?: string; ticker?: string } | null;
  live_stake: string | null;
  voting_power: string | null;
}

export interface PoolVoteRow {
  proposal_id: string;
  proposal_tx_hash: string;
  proposal_index: number;
  vote_tx_hash: string;
  block_time: number;
  vote: KoiosVote;
  meta_url: string | null;
  meta_hash: string | null;
}

/* ------------------------------------------------------------------------- */
/* Transactions                                                               */
/* ------------------------------------------------------------------------- */

export interface TxStatusRow {
  tx_hash: string;
  num_confirmations: number | null;
}

export interface TxVotingProcedure {
  vote: KoiosVote;
  voter: string;
  voter_hex: string;
  voter_role: KoiosVoterRole;
  proposal_index: number;
  proposal_tx_hash: string;
}

export interface TxProposalProcedure {
  type: KoiosProposalType;
  index: number;
  deposit: string | null;
  meta_url: string | null;
  meta_hash: string | null;
  return_address: string | null;
}

export interface TxCertificate {
  index: number;
  type: string;
  info: Record<string, unknown> | null;
}

export interface TxInfoRow {
  tx_hash: string;
  block_hash: string;
  block_height: number | null;
  epoch_no: number;
  absolute_slot: number;
  tx_timestamp: number;
  certificates: TxCertificate[] | null;
  voting_procedures: TxVotingProcedure[] | null;
  proposal_procedures: TxProposalProcedure[] | null;
}
