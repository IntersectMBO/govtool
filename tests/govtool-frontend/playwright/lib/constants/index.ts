export const SECURITY_RELEVANT_PARAMS_MAP: Record<string, string> = {
  maxBlockBodySize: "max_block_size",
  maxTxSize: "max_tx_size",
  maxBlockHeaderSize: "max_bh_size",
  maxValueSize: "max_val_size",
  maxBlockExecutionUnits: "max_block_ex_mem",
  txFeePerByte: "min_fee_a",
  txFeeFixed: "min_fee_b",
  utxoCostPerByte: "coins_per_utxo_size",
  govActionDeposit: "gov_action_deposit",
  minFeeRefScriptCostPerByte: "min_fee_ref_script_cost_per_byte",
};

export const BOOTSTRAP_PROPOSAL_TYPE_FILTERS = ["Info Action"];

export const PROPOSAL_STATUS_FILTER = ["Submitted for vote", "Active proposal"];

export const guardrailsScript = {
  type: "PlutusScriptV3",
  description: "",
  cborHex: "46450101004981",
};

export const guardrailsScriptHash =
  "914d97d63e2b7113465739faddd82362b1deaeedbcc4d01016c35c6e";

export const outcomeStatusType = [
  "Expired",
  "Not Ratified",
  "Ratified",
  "Enacted",
  "Live",
];
