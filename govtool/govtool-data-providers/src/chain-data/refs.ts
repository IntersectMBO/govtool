/**
 * Chain Data API — how entities are addressed.
 *
 * One bech32 identifier per entity, in the current standard form (SPEC.md §3.1).
 * No legacy alternates — CIP-105, raw hex, `txHash#index` and shortened display
 * forms are a consumer's business, not this contract's.
 */

import type { Bech32, Hex } from './common';

export type VoterRole = 'drep' | 'spo' | 'cc';

/**
 * A reference to something that can cast a governance vote.
 *
 * `id` is the entity's STABLE identifier:
 *   drep  CIP-129 DRep id
 *   spo   `pool1…`
 *   cc    CIP-129 **cold** credential — never the hot one, which rotates
 */
export interface VoterRef {
  role: VoterRole;
  id: Bech32;
  /**
   * Optional: derivable from the CIP-129 header byte, which encodes key vs
   * script. A provider that has it cheaply supplies it; one that does not omits
   * it rather than guessing.
   */
  isScriptBased?: boolean;
}

/**
 * A committee member's hot credential, as it appears on a vote.
 *
 * A vote's on-chain voter field for a committee member is the HOT credential;
 * resolving it to the cold one is a join. So `cold` is optional here — but
 * PROVIDERS SHOULD SUPPLY IT. There is no lookup to fall back on, and a
 * consumer that does not receive it shows no voter information for that vote.
 */
export interface CommitteeVoterRef {
  role: 'cc';
  hot: Bech32;
  cold?: Bech32;
  isScriptBased?: boolean;
}

/** The voter on a vote row: a DRep or pool by stable id, or a committee hot key. */
export type VoteCastBy = VoterRef | CommitteeVoterRef;

export interface GovActionRef {
  /** CIP-129 governance action id. */
  id: Bech32;
  /** The transaction that submitted it, and the index within it. */
  txHash: Hex;
  index: number;
}

export type VoteChoice = 'yes' | 'no' | 'abstain';

/**
 * Governance action PURPOSES. `prevGovActionId` is per purpose, not per type —
 * `UpdateCommittee` and `NoConfidence` share the `committee` lineage, so the
 * enacted head of either is the same action. A per-type implementation returns
 * the wrong id and the ledger rejects the transaction.
 */
export type GovActionLineage =
  'pparamUpdate' | 'hardFork' | 'committee' | 'constitution';

/** The predefined delegation targets. They are NOT DReps. */
export type PredefinedDelegation = 'alwaysAbstain' | 'alwaysNoConfidence';

/**
 * Where an account's governance stake is delegated.
 *
 * `alwaysAbstain` and `alwaysNoConfidence` have no credential, anchor or
 * registration, so they are a separate variant rather than a DRep with special
 * values.
 */
export type DelegationTarget =
  | { kind: 'drep'; drep: VoterRef }
  | { kind: 'predefined'; target: PredefinedDelegation };

export interface AccountRef {
  stakeAddress: Bech32;
  stakeKeyHash: Hex;
}
