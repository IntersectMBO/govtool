import { EpochParams } from "./api";
import { MetadataValidationStatus } from "./metadataValidation";

// Governance action records served by the GovTool backend.

export type GovernanceActionStatus = {
  ratified_epoch: number | null;
  enacted_epoch: number | null;
  dropped_epoch: number | null;
  expired_epoch: number | null;
};

export type GovernanceActionStatusTimes = {
  ratified_time: string | null;
  enacted_time: string | null;
  dropped_time: string | null;
  expired_time: string | null;
};

export type GovernanceActionJSONValue =
  | string
  | number
  | boolean
  | null
  | { [property: string]: GovernanceActionJSONValue }
  | GovernanceActionJSONValue[];

export type GovernanceActionDescription = {
  [key: string]: GovernanceActionJSONValue;
};

export type GovernanceActionRecord = {
  id: string;
  tx_hash: string;
  index: number;
  type: string;
  description: GovernanceActionDescription | null;
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
  status: GovernanceActionStatus;
  status_times: GovernanceActionStatusTimes;
  motivation?: string;
  rationale?: string;
  yes_votes: number | null;
  no_votes: number | null;
  abstain_votes: number | null;
  pool_yes_votes: number | null;
  pool_no_votes: number | null;
  pool_abstain_votes: number | null;
  cc_yes_votes: number | null;
  cc_no_votes: number | null;
  cc_abstain_votes: number | null;
  vote_aggregates?: GovernanceActionVoteAggregate[];
  prev_gov_action_index: string | number | null;
  prev_gov_action_tx_hash: string | null;
};

/** Values use lovelace, member counts, or 0..1 fractions as declared. */
export type GovernanceActionVoteAggregate = {
  role: "drep" | "spo" | "cc";
  representation: "stake" | "count" | "percent";
  yes: string;
  no: string;
  abstain: string;
  notVoted: string;
  totalEligible: string;
  threshold: { numerator: number; denominator: number };
  passing?: boolean;
};

export type GovernanceActionReference = {
  "@type": string;
  label: string;
  uri: string;
};

export type GovernanceActionMetadata = {
  metadataStatus?: MetadataValidationStatus;
  metadataValid: boolean;
  data: {
    abstract?: string;
    comment?: string;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    externalUpdates?: any[];
    motivation?: string;
    rationale?: string;
    references?: GovernanceActionReference[];
    title: string;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    authors: any[];
  };
};

export type GovernanceActionAuthorWitness = {
  witnessAlgorithm: string;
  publicKey: string;
  signature: string;
};

export type GovernanceActionSignatureVerificationDto = {
  author: {
    name: string;
    witness: GovernanceActionAuthorWitness;
  };
  metadataUrl: string;
};

export type GovernanceActionSignatureVerificationResult = {
  isValid: boolean;
  author: string;
  message?: string;
  error?: string;
};

/** The pdf API's proposal item, from `/api/proposals`. */
export type GovernanceActionProposalDiscussion = {
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
