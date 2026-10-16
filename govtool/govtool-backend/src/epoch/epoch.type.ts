import type { CostModels } from '@govtool/data-providers/chain-data';
import type { ApiInteger } from 'src/common/integer';

/**
 * The protocol-parameter columns db-sync's `epoch_param` and `param_proposal`
 * tables share, in their own snake_case names. The frontend reads these names
 * directly: `/epoch/params` for the parameters in force, and a proposal's
 * `protocolParams` for what a ParameterChange proposes, which it diffs key by
 * key against `/epoch/params`.
 *
 * A column is null when the source object does not set it: on `/epoch/params`
 * only for a parameter the ledger no longer has, on a proposal for every
 * parameter the action leaves unchanged. Every key is always present — the
 * frontend tests `!== null`, so an absent key reads as a change.
 */
export type LegacyParamColumns = {
  min_fee_a: number | null;
  min_fee_b: number | null;
  max_block_size: number | null;
  max_tx_size: number | null;
  max_bh_size: number | null;
  key_deposit: ApiInteger | null;
  pool_deposit: ApiInteger | null;
  max_epoch: number | null;
  optimal_pool_count: number | null;
  influence: number | null;
  monetary_expand_rate: number | null;
  treasury_growth_rate: number | null;
  /** 0 on `/epoch/params` from Babbage on, where the parameter no longer exists. */
  decentralisation: 0 | null;
  protocol_major: number | null;
  protocol_minor: number | null;
  /** 0 on `/epoch/params` from Babbage on, where the parameter no longer exists. */
  min_utxo_value: 0 | null;
  min_pool_cost: ApiInteger | null;
  price_mem: number | null;
  price_step: number | null;
  max_tx_ex_mem: number | null;
  max_tx_ex_steps: number | null;
  max_block_ex_mem: number | null;
  max_block_ex_steps: number | null;
  max_val_size: number | null;
  collateral_percent: number | null;
  max_collateral_inputs: number | null;
  coins_per_utxo_size: ApiInteger | null;
  pvt_motion_no_confidence: number | null;
  pvt_committee_normal: number | null;
  pvt_committee_no_confidence: number | null;
  pvt_hard_fork_initiation: number | null;
  pvtpp_security_group: number | null;
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
  committee_min_size: number | null;
  committee_max_term_length: number | null;
  gov_action_lifetime: number | null;
  gov_action_deposit: ApiInteger | null;
  drep_deposit: ApiInteger | null;
  drep_activity: number | null;
  min_fee_ref_script_cost_per_byte: number | null;
  /**
   * The joined `cost_model` row. The frontend reads `costs` to compute a
   * script data hash and to show a cost-model change; `id` and `hash` are
   * db-sync bookkeeping and stay null.
   */
  cost_model: { id: null; costs: CostModels; hash: null } | null;
};

/**
 * The legacy `/epoch/params` body: db-sync's `epoch_param` row. Every protocol
 * parameter comes from the contract's `ProtocolParams`, which carries the full
 * set (D136). The bookkeeping columns the contract drops on purpose (D1) and
 * `extra_entropy`, which the ledger no longer has, are null.
 */
export type LegacyEpochParams = LegacyParamColumns & {
  id: null;
  epoch_no: number;
  nonce: null;
  cost_model_id: null;
  block_id: null;
  extra_entropy: null;
};

/**
 * A ParameterChange proposal's `protocolParams`: db-sync's `param_proposal`
 * row, where only the proposed parameters are set. Its bookkeeping columns are
 * null, `epoch_no` included — it is not a parameter, and the frontend's diff
 * view, which filters only `id`, `key` and `registered_tx_id`, would otherwise
 * list it as one.
 */
export type LegacyParamProposal = LegacyParamColumns & {
  id: null;
  epoch_no: null;
  key: null;
  entropy: null;
  cost_model_id: null;
  registered_tx_id: null;
};
