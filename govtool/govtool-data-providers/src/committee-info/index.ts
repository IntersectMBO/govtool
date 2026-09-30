/**
 * Committee Info Provider v1 — who a committee credential belongs to. Optional
 * (SPEC.md §8).
 *
 * This cannot be part of the metadata service, because A COMMITTEE MEMBER HAS
 * NO ANCHOR. Every other off-chain document in this system is pointed at by the
 * chain — DRep (CIP-119), action (CIP-108), vote (CIP-100), constitution — which
 * is what makes one resolver possible. An `UpdateCommittee` action names cold
 * credentials and nothing else, so identity must come from a curated source.
 *
 * No public registry or CIP-backed schema for this exists; every field beyond
 * the join key is therefore optional and best-effort.
 */

import type { Bech32, Timestamp } from '../chain-data/common';

export interface CommitteeMemberInfo {
  /** The only stable join key — cold, never hot, which rotates. */
  coldCredentialId: Bech32;
  name?: string;
  organisation?: string;
  /** A URL, not inlined: a member is a person, and a portrait is worth a fetch. */
  avatarUrl?: string;
  country?: string;
  bio?: string;
  contactLinks?: { label: string; url: string }[];
  /**
   * Provenance — e.g. `intersect`, `self-submitted`, `curated`. With no
   * authoritative registry, this is the only basis a consumer has for judging
   * how much to trust a name.
   */
  source?: string;
  lastUpdatedAt?: Timestamp;
}

export interface CommitteeInfoProviderV1 {
  /** `null` = no curated record. The normal case, not an error. */
  getMemberInfo(coldCredentialId: string): Promise<CommitteeMemberInfo | null>;
}
