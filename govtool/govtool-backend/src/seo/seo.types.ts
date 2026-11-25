import type {
  GovernanceActionStatus,
  GovernanceActionStatusTimes,
} from '../governance-actions/governance-actions.type';

/**
 * What the SEO pages read about an action, DRep or forum proposal: only what
 * is already stored, so a page load never waits on IPFS or the forum.
 */

export type SeoAction = {
  cip129Id: string;
  /** The legacy type name, e.g. InfoAction, NewCommittee. */
  type: string;
  title: string | null;
  abstract: string | null;
  expiryDate: string | null;
  expiryEpoch: number | null;
  createdDate: string | null;
  status: GovernanceActionStatus;
  statusTimes: GovernanceActionStatusTimes;
};

export type SeoDRep = {
  /** The CIP-105 view the directory links with. */
  view: string;
  givenName: string | null;
  objectives: string | null;
  status: 'Active' | 'Inactive' | 'Retired';
  /** Lovelace; null when the provider has no figure. */
  votingPower: bigint | null;
  doNotList: boolean;
  metadataError: string | null;
  /** Whether the DRep anchored a document at all. */
  hasMetadata: boolean;
  latestRegistrationDate: string;
};

export type SeoForumProposal = {
  id: string;
  title: string;
  abstract: string | null;
  updatedAt: string | null;
};
