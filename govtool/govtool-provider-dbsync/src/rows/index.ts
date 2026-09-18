/**
 * Row shapes as `pg` returns them for each bundled statement — snake_case,
 * with `bigint`/`numeric` columns as strings, `int` columns as numbers and
 * `timestamp` columns as `Date`. Ported from the legacy backend's `*.type.ts`
 * so a mapper here can be checked line by line against the mapper there.
 */

export type DbNumber = number | string;

/* get-voting-power.sql */
export interface DRepVotingPowerRow {
  amount: DbNumber;
}

/* get-dreps-voting-power-list.sql, get-filtered-dreps-voting-power.sql */
export interface DRepVotingPowerListRow {
  view: string;
  /**
   * NULL for the predefined options: `drep_hash.raw` is NULL for
   * `drep_always_abstain` and `drep_always_no_confidence`.
   *
   * Note the statement is `DISTINCT ON (raw)`, so both collapse to a single
   * row — the legacy API returned only one of the two, and that is preserved
   * because the SQL is frozen.
   */
  hash_raw: string | null;
  voting_power: DbNumber;
  given_name: string | null;
}

/* get-drep-info.sql */
export interface DRepInfoRow {
  is_script_based: boolean;
  is_registered_as_drep: boolean | null;
  was_registered_as_drep: boolean | null;
  is_registered_as_sole_voter: boolean | null;
  was_registered_as_sole_voter: boolean | null;
  deposit: DbNumber | null;
  url: string | null;
  data_hash: string | null;
  voting_power: DbNumber | null;
  drep_register_tx_hash: string | null;
  drep_retire_tx_hash: string | null;
  sole_voter_register_tx_hash: string | null;
  sole_voter_retire_tx_hash: string | null;
  payment_address: string | null;
  given_name: string | null;
  objectives: string | null;
  motivations: string | null;
  qualifications: string | null;
  image_url: string | null;
  image_hash: string | null;
}

/* list-dreps.sql */
export interface DRepListRow {
  drep_hash: string;
  view: string;
  has_script: boolean;
  url: string | null;
  metadata_hash: string | null;
  deposit: DbNumber;
  amount: DbNumber | null;
  active: boolean;
  tx_hash: string | null;
  last_register_time: Date | string;
  latest_deposit: DbNumber;
  has_non_deregister_voting_anchor: boolean | null;
  fetch_error: string | null;
  payment_address: string | null;
  given_name: string | null;
  objectives: string | null;
  motivations: string | null;
  qualifications: string | null;
  image_url: string | null;
  image_hash: string | null;
  votes_last_year: DbNumber | null;
  identity_references: unknown;
  link_references: unknown;
}

/**
 * get-votes.sql, read positionally. Its SELECT list has three unaliased
 * `encode(...)` columns and an unaliased `CONCAT`, so by name the row would
 * be `{ gov_action_proposal_id, concat, encode, lower, url, encode, ... }`
 * with the DRep hash overwritten by the anchor hash. The Haskell backend
 * always read this statement by position; so does this provider.
 */
export type DRepVoteTuple = [
  proposalId: DbNumber,
  govActionId: string,
  drepId: string,
  vote: string,
  url: string | null,
  docHash: string | null,
  epochNo: DbNumber,
  date: Date | string,
  voteTxHash: string,
];

/* list-proposals.sql */
export interface ProposalRow {
  id: DbNumber;
  tx_hash: string;
  index: DbNumber;
  type: string;
  description: unknown;
  expiry_date: Date | string | null;
  expiration: DbNumber | null;
  time: Date | string;
  epoch_no: DbNumber;
  url: string | null;
  data_hash: string | null;
  proposal_params: unknown | null;
  title: string | null;
  abstract: string | null;
  motivation: string | null;
  rationale: string | null;
  yes_votes: DbNumber;
  no_votes: DbNumber;
  abstain_votes: DbNumber;
  pool_yes_votes: DbNumber;
  pool_no_votes: DbNumber;
  pool_abstain_votes: DbNumber;
  cc_yes_votes: DbNumber;
  cc_no_votes: DbNumber;
  cc_abstain_votes: DbNumber;
  prev_gov_action_index: DbNumber | null;
  prev_gov_action_tx_hash: string | null;
  json_content: unknown | null;
  authors: unknown | null;
}

/* get-previous-enacted-governance-action-proposal-details.sql */
export interface EnactedProposalDetailsRow {
  id: DbNumber;
  tx_id: DbNumber;
  index: DbNumber;
  description: unknown | null;
  hash: string;
}

/* get-network-info.sql */
export interface NetworkInfoRow {
  current_epoch: DbNumber | null;
  current_block: DbNumber | null;
  network_name: string | null;
}

/* get-network-total-stake.sql */
export interface NetworkTotalStakeRow {
  total_stake_controlled_by_active_dreps: DbNumber;
  total_stake_controlled_by_spos: DbNumber;
  always_abstain_voting_power: DbNumber;
  always_no_confidence_voting_power: DbNumber;
}

/* get-network-metrics.sql */
export interface NetworkMetricsRow {
  unique_delegators: DbNumber;
  total_delegations: DbNumber;
  total_gov_action_proposals: DbNumber;
  total_drep_votes: DbNumber;
  total_registered_dreps: DbNumber;
  total_drep_distr: DbNumber | null;
  total_active_dreps: DbNumber;
  total_inactive_dreps: DbNumber;
  total_active_cip119_compliant_dreps: DbNumber;
  total_registered_direct_voters: DbNumber;
  no_of_committee_members: DbNumber;
  quorum_numerator: DbNumber | null;
  quorum_denominator: DbNumber | null;
}

/* get-account-info.sql */
export interface AccountInfoRow {
  id: DbNumber;
  view: string;
  is_script_based: boolean;
  is_registered: boolean;
}

/* get-current-delegation.sql — the tx hash column is unaliased, hence `encode`. */
export interface CurrentDelegationRow {
  drep_raw: string | null;
  drep_view: string;
  has_script: boolean;
  encode: string;
}

/* get-stake-key-voting-power.sql */
export interface StakeKeyVotingPowerRow {
  total_balance: DbNumber;
  stake_address: string;
}

/* get-transaction-status.sql */
export interface TransactionStatusRow {
  tx_exists: boolean;
  voting_procedures: unknown[] | null;
}

/* get-survey-definition.sql */
export interface SurveyDefinitionRow {
  payload_cbor_hex: string;
}

/* get-current-epoch-params.sql */
export interface EpochParamsRow {
  epoch_param: Record<string, unknown> | null;
}
