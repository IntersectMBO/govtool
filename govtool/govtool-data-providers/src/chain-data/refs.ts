/**
 * Chain Data API — cross-module identity shapes.
 *
 * These are the only types every module is allowed to reference, which is what
 * keeps `accounts`, `governance/*` and `transactions` from importing each
 * other's full entities. A module that needs to *point at* a DRep or a
 * governance action uses the ref; only the owning module returns the entity.
 */

import type { Bech32, Hex } from './common';

export type VoterRole = 'drep' | 'spo' | 'cc' | 'direct';

/** Identity of anything that can cast a governance vote. */
export interface VoterRef {
  role: VoterRole;
  /**
   * CIP-129 bech32 (`drep1…`, `cc_hot1…`, `pool1…`). Canonical id used in URLs.
   * A provider whose source stores only the raw credential computes it.
   */
  id: Bech32;
  hash: Hex;
  isScriptBased: boolean;
  /**
   * The pre-CIP-129 bech32 of the same credential — `drep1` + hash with no
   * credential-type header — as db-sync's `drep_hash.view` and the legacy
   * GovTool API expose it. Present when the provider has it, so a consumer
   * that still speaks CIP-105 reads this instead of re-encoding `hash`.
   */
  cip105Id?: Bech32;
}

export interface GovActionRef {
  /** CIP-129 `gov_action1…`; canonical id used in URLs. */
  id: Bech32;
  txHash: Hex;
  index: number;
  /** Provider-native opaque identifier, for compatibility only; see `TxRef.providerId`. */
  providerId?: string;
}

export type VoteChoice = 'yes' | 'no' | 'abstain';

/**
 * The two ledger-defined delegation targets that are not a real DRep.
 *
 * They have no credential: no hash, and so no CIP-129 id. Anything that can
 * name one alongside real DReps — a delegation, a voting-power listing —
 * discriminates on `kind` rather than trying to express them as a `VoterRef`.
 */
export type PredefinedDelegation = 'alwaysAbstain' | 'alwaysNoConfidence';

/** A DRep credential, or one of the predefined options. */
export type DRepTarget =
  | { kind: 'drep'; drep: VoterRef }
  | {
      kind: 'predefined';
      option: PredefinedDelegation;
      /**
       * The source's own name for it (db-sync's `drep_always_abstain` /
       * `drep_always_no_confidence`), when it has one.
       */
      view?: string;
    };

/** Stake account identity. */
export interface AccountRef {
  stakeAddress: Bech32;
  stakeKeyHash: Hex;
  isScriptBased: boolean;
}
