import type { CurrentDelegation } from "./api";

// Shapes the header's global search reads.

/** One row of the forum backend's GET /api/proposals. */
export type ForumProposal = {
  id: number;
  attributes: {
    createdAt?: string;
    content: {
      attributes: {
        prop_name?: string;
        prop_abstract?: string | null;
        prop_submitted?: boolean;
        createdAt?: string;
        gov_action_type?: {
          attributes?: { gov_action_type_name?: string };
        } | null;
      };
    } | null;
  };
};

/** What GovTool knows about a stake address. */
export type StakeAddressLookup = {
  stakeAddress: string;
  /** Undefined when the backend could not say. */
  isRegistered?: boolean;
  delegation?: CurrentDelegation | null;
  votingPower?: number;
};
