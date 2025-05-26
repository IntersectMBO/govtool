import { faker } from "@faker-js/faker";
import { InvalidMetadataType } from "@types";

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

export const InvalidMetadata: InvalidMetadataType[] = [
  {
    type: "Data Formatted Incorrectly",
    reason: "hash is valid but incorrect metadata format.",
    url: "https://metadata-govtool.cardanoapi.io/data/Lolita",
    hash: "62a37df07103f0a69690c8975700e06b7c3c3069cb3d105abec00e820e831dda",
  },
  {
    type: "Data Missing",
    reason: "metadata URL could not be found.",
    url: faker.internet.url() + "/test.jsonld",
    hash: "99a19b124ceb89bbd92354e8d11f913d1aec7280ce19ac4c1c6cc72f0ea91884",
  },
  {
    type: "Data Not Verifiable",
    reason: "metadata hash and URL do not match.",
    url: "https://metadata-govtool.cardanoapi.io/data/data.jsonld",
    hash: "e71bf6171adda3754a87fff5c2d8d9e404eb3366428a5be13f7e76357a39004f",
  },
  {
    type: "Data Not Verifiable",
    reason: "metadata hash and URL do not match and is incorrect ga format",
    url: "https://metadata-govtool.cardanoapi.io/data/Lolita",
    hash: "e71bf6171adda3754a87fff5c2d8d9e404eb3366428a5be13f7e76357a39004f",
  },
];

export const connectToCardanoWalletSection = [
  {
    testId: "home-card-discuss-budget-proposals",
    urlPattern: /\/budget_discussion/,
    label: "Discuss Budget Proposals",
  },
  {
    testId: "home-card-create-a-budget-proposal",
    urlPattern: /\/budget_discussion/,
    label: "Create a Budget Proposal",
  },
  {
    testId: "home-card-discuss-governance-actions",
    urlPattern: /\/proposal_discussion/,
    label: "Discuss Governance Actions",
  },
  {
    testId: "home-card-propose-a-governance-action",
    urlPattern: /\/proposal_discussion\/propose/,
    label: "Propose a Governance Action",
  },
  {
    testId: "home-card-register-to-vote",
    urlPattern: /\/register_direct_voter/,
    label: "Register to Vote",
  },
  {
    testId: "home-card-delegate-your-vote-to-a-drep",
    urlPattern: /\/drep_directory/,
    label: "Delegate Your Vote to a DRep",
  },
  {
    testId: "home-card-become-a-drep",
    urlPattern: /\/register_drep/,
    Label: "Become a DRep",
  },
  {
    testId: "home-card-vote-on-governance-actions",
    urlPattern: /\governance_actions/,
    label: "Vote on Governance Actions",
  },
];
