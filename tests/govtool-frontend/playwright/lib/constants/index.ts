import {
  incorrectFormatFixture,
  missingMetadataUrl,
  UNMATCHED_METADATA_HASH,
  validCip108Fixture,
} from "@helpers/invalidMetadataFixtures";
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

export const actionRecordStatusType = [
  "Expired",
  "Not Ratified",
  "Ratified",
  "Enacted",
  "Live",
];

// Anchors are uploaded by ensureInvalidMetadataFixtures(); call it in a
// beforeAll of any test that uses them.
export const InvalidMetadata: InvalidMetadataType[] = [
  {
    type: "Data Formatted Incorrectly",
    reason: "hash is valid but incorrect metadata format.",
    url: incorrectFormatFixture.url,
    hash: incorrectFormatFixture.hash,
  },
  {
    type: "Data Missing",
    reason: "metadata URL could not be found.",
    url: missingMetadataUrl,
    hash: UNMATCHED_METADATA_HASH,
  },
  {
    type: "Data Not Verifiable",
    reason: "metadata hash and URL do not match.",
    url: validCip108Fixture.url,
    hash: UNMATCHED_METADATA_HASH,
  },
  {
    type: "Data Not Verifiable",
    reason: "metadata hash and URL do not match and is incorrect ga format",
    url: incorrectFormatFixture.url,
    hash: UNMATCHED_METADATA_HASH,
  },
];
