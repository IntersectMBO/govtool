import { EpochParams } from "./api";
import { MetadataValidationStatus } from "./metadataValidation";

// Shapes served by the outcomes API (VITE_OUTCOMES_API_URL), which answers in
// db-sync column names rather than the camelCase of the main backend.

export type OutcomeStatus = {
  ratified_epoch: number | null;
  enacted_epoch: number | null;
  dropped_epoch: number | null;
  expired_epoch: number | null;
};

export type OutcomeStatusTimes = {
  ratified_time: string | null;
  enacted_time: string | null;
  dropped_time: string | null;
  expired_time: string | null;
};

export type OutcomeJSONValue =
  | string
  | number
  | boolean
  | null
  | { [property: string]: OutcomeJSONValue }
  | OutcomeJSONValue[];

export type OutcomeGovActionDescription = {
  [key: string]: OutcomeJSONValue;
};

export type OutcomeGovernanceAction = {
  id: string;
  tx_hash: string;
  index: number;
  type: string;
  description: OutcomeGovActionDescription | null;
  expiry_date: string;
  expiration: number;
  time: string;
  epoch_no: number;
  url: string;
  data_hash: string;
  proposal_params: EpochParams | null;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  json_metadata: any | null;
  title?: string;
  abstract?: string;
  status: OutcomeStatus;
  status_times: OutcomeStatusTimes;
  motivation?: string;
  rationale?: string;
  yes_votes: number;
  no_votes: number;
  abstain_votes: number;
  pool_yes_votes: number;
  pool_no_votes: number;
  pool_abstain_votes: number;
  cc_yes_votes: number;
  cc_no_votes: number;
  cc_abstain_votes: number;
  prev_gov_action_index: string | number | null;
  prev_gov_action_tx_hash: string | null;
};

export type OutcomeReference = {
  "@type": string;
  label: string;
  uri: string;
};

export type OutcomeGovActionMetadata = {
  metadataStatus?: MetadataValidationStatus;
  metadataValid: boolean;
  data: {
    abstract?: string;
    comment?: string;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    externalUpdates?: any[];
    motivation?: string;
    rationale?: string;
    references?: OutcomeReference[];
    title: string;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    authors: any[];
  };
};

export type OutcomeNetworkMetrics = {
  epoch_no: number;
  /** Lovelace */
  total_stake_controlled_by_active_dreps: string;
  /** Lovelace */
  total_stake_controlled_by_stake_pools: string;
  /** Lovelace */
  always_abstain_voting_power: string;
  /** Lovelace */
  spos_abstain_voting_power: string;
  /** Lovelace */
  always_no_confidence_voting_power: string;
  /** Lovelace */
  spos_no_confidence_voting_power: string;
  no_of_committee_members: number;
  quorum_numerator: number;
  quorum_denominator: number;
};

export type OutcomeAuthorWitness = {
  witnessAlgorithm: string;
  publicKey: string;
  signature: string;
};

export type OutcomeSignatureVerificationDto = {
  author: {
    name: string;
    witness: OutcomeAuthorWitness;
  };
  metadataUrl: string;
};

export type OutcomeSignatureVerificationResult = {
  isValid: boolean;
  author: string;
  message?: string;
  error?: string;
};

/** The pdf API's proposal item, as `/governance-actions/proposal/:txHash` forwards it. */
export type OutcomeProposalDiscussion = {
  id: number | string;
  attributes?: {
    createdAt?: string;
    content?: {
      attributes?: {
        gov_action_type?: {
          attributes?: { gov_action_type_name?: string };
        };
      };
    };
  };
};
