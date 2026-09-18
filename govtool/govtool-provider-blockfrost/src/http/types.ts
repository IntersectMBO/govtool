/**
 * Response shapes of the Blockfrost HTTP API, as this provider consumes them.
 *
 * Only the fields actually read are declared. Field names are Blockfrost's
 * (snake_case), and every numeric quantity that is lovelace or larger than a
 * 32-bit integer arrives as a decimal `string` — which is what the contract
 * wants anyway, so those are passed through untouched.
 *
 * Verified against blockfrost-ryo 3.1.1 on mainnet, 2026-09-18.
 */

/** `GET /` */
export interface BfRoot {
  url: string;
  version: string;
}

/** `GET /health` */
export interface BfHealth {
  is_healthy: boolean;
}

/** `GET /genesis` */
export interface BfGenesis {
  active_slots_coefficient: number;
  update_quorum: number;
  max_lovelace_supply: string;
  network_magic: number;
  epoch_length: number;
  system_start: number;
  slots_per_kes_period: number;
  slot_length: number;
  max_kes_evolutions: number;
  security_param: number;
}

/** `GET /blocks/latest` */
export interface BfBlock {
  time: number;
  height: number | null;
  hash: string;
  slot: number | null;
  epoch: number | null;
  epoch_slot: number | null;
  tx_count: number;
}

/** `GET /epochs/latest`, `GET /epochs/{n}` */
export interface BfEpoch {
  epoch: number;
  start_time: number;
  end_time: number;
  first_block_time: number;
  last_block_time: number;
  block_count: number;
  tx_count: number;
  output: string;
  fees: string;
  active_stake: string | null;
}

/**
 * `GET /epochs/{n}/parameters`
 *
 * Indexed rather than fully enumerated: the whole object is handed to the
 * contract as `ProtocolParams.raw`, and only the governance-relevant fields
 * are lifted out by name.
 */
export interface BfEpochParameters {
  epoch: number;
  [key: string]: unknown;
}

/** `GET /governance/dreps` — the directory is ids only. */
export interface BfDRepRef {
  drep_id: string;
  hex: string;
}

/** `GET /governance/dreps/{id}` */
export interface BfDRep {
  drep_id: string;
  hex: string;
  amount: string;
  active: boolean;
  active_epoch: number | null;
  has_script: boolean;
  retired: boolean;
  expired: boolean;
  last_active_epoch: number | null;
}

/** `GET /governance/dreps/{id}/metadata` — 404 when the DRep has no anchor. */
export interface BfDRepMetadata {
  drep_id: string;
  hex: string;
  url: string;
  hash: string;
  json_metadata: unknown;
  bytes?: string;
}

/** `GET /governance/dreps/{id}/delegators` */
export interface BfDRepDelegator {
  address: string;
  amount: string;
}

/** `GET /governance/dreps/{id}/updates` */
export interface BfDRepUpdate {
  tx_hash: string;
  cert_index: number;
  action: 'registered' | 'updated' | 'deregistered' | string;
}

/**
 * `GET /governance/dreps/{id}/votes`
 *
 * `tx_hash` + `cert_index` locate the **vote**, not the proposal: one
 * transaction carries several votes at successive indices. The proposal the
 * vote was cast on is not part of this response, which is why a DRep's voting
 * record cannot be assembled from it — see the README.
 */
export interface BfDRepVote {
  tx_hash: string;
  cert_index: number;
  vote: string;
}

/** `GET /governance/proposals` */
export interface BfProposalRef {
  tx_hash: string;
  cert_index: number;
  governance_type: string;
}

/** `GET /governance/proposals/{tx}/{index}` */
export interface BfProposal extends BfProposalRef {
  /** The ledger's own description: `{ tag, contents }`. `tag` is the ledger name. */
  governance_description: BfGovernanceDescription | null;
  deposit: string;
  return_address: string;
  ratified_epoch: number | null;
  enacted_epoch: number | null;
  dropped_epoch: number | null;
  expired_epoch: number | null;
  expiration: number | null;
}

export interface BfGovernanceDescription {
  tag: string;
  contents?: unknown;
}

/** `GET /governance/proposals/{tx}/{index}/metadata` — 404 when there is no anchor. */
export interface BfProposalMetadata {
  tx_hash: string;
  cert_index: number;
  url: string;
  hash: string;
  json_metadata: unknown;
  bytes?: string;
}

/** `GET /governance/proposals/{tx}/{index}/parameters` — 404 unless ParameterChange. */
export interface BfProposalParameters {
  tx_hash: string;
  cert_index: number;
  /** Every parameter, `null` for the ones this action does not change. */
  parameters: Record<string, unknown>;
}

/** `GET /governance/proposals/{tx}/{index}/withdrawals` — `[]` unless TreasuryWithdrawals. */
export interface BfProposalWithdrawal {
  stake_address: string;
  amount: string;
}

/** `GET /governance/proposals/{tx}/{index}/votes` */
export interface BfProposalVote {
  tx_hash: string;
  cert_index: number;
  voter_role: string;
  voter: string;
  vote: string;
}

/** `GET /accounts/{stakeAddress}` */
export interface BfAccount {
  stake_address: string;
  active: boolean;
  active_epoch: number | null;
  controlled_amount: string;
  rewards_sum: string;
  withdrawals_sum: string;
  reserves_sum: string;
  treasury_sum: string;
  withdrawable_amount: string;
  pool_id: string | null;
  /** The governance delegation. `null` when the account has never delegated. */
  drep_id: string | null;
}

/** `GET /accounts/{stakeAddress}/delegations` — stake-pool delegation history. */
export interface BfAccountDelegation {
  active_epoch: number;
  tx_hash: string;
  amount: string;
  pool_id: string;
}

/** `GET /accounts/{stakeAddress}/registrations` */
export interface BfAccountRegistration {
  tx_hash: string;
  action: 'registered' | 'deregistered' | string;
}
