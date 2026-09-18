/**
 * Chain Data API — cross-module identity shapes.
 *
 * These are the only types every module is allowed to reference, which is what
 * keeps `accounts`, `governance/*` and `transactions` from importing each
 * other's full entities. A module that needs to *point at* a DRep or a
 * governance action uses the ref; only the owning module returns the entity.
 */

import type { Bech32, Hex } from "./common";

export type VoterRole = "drep" | "spo" | "cc" | "direct";

/** Identity of anything that can cast a governance vote. */
export interface VoterRef {
  role: VoterRole;
  /** CIP-129 bech32 (drep1…, cc_hot1…, pool1…). Canonical id used in URLs. */
  id: Bech32;
  hash: Hex;
  isScriptBased: boolean;
}

export interface GovActionRef {
  /** CIP-129 `gov_action1…`; canonical id used in URLs. */
  id: Bech32;
  txHash: Hex;
  index: number;
}

export type VoteChoice = "yes" | "no" | "abstain";

/** Stake account identity. */
export interface AccountRef {
  stakeAddress: Bech32;
  stakeKeyHash: Hex;
  isScriptBased: boolean;
}
