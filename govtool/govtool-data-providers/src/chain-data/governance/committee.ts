/**
 * Chain Data API — `/governance/committee`
 *
 * Committee membership is genuine LEDGER STATE, not something readable off the
 * latest action. `UpdateCommittee` actions are add/remove deltas, so current
 * membership is assembled from the genesis committee, every enacted
 * `UpdateCommittee`, any enacted `NoConfidence`, the `AuthCommitteeHotCert` and
 * `ResignCommitteeColdCert` certificates, and term expiry against the current
 * epoch.
 *
 * The constitution, by contrast, IS derivable: each `NewConstitution` action
 * replaces the previous one outright and its body carries the anchor, so
 * `getEnacted('constitution')` → `body.anchor` is sufficient.
 */

import type {
  Anchor,
  Bech32,
  EpochNo,
  EpochStamp,
  Envelope,
  Hex,
  Ratio,
} from '../common';
import type { GovActionRef } from '../refs';

export interface CommitteeMember {
  role: 'cc';
  /** The member's identity. Never the hot credential, which rotates. */
  coldCredential: Bech32;
  /** Absent until the member authorises one; changes over the seat's life. */
  hotCredential: Bech32 | null;
  termStartEpoch: EpochNo | null;
  termExpiryEpoch: EpochNo | null;
  hasResigned: boolean;
  isScriptBased?: boolean;
}

export interface Committee {
  members: CommitteeMember[];
  quorum: Ratio;
  /** The action that last set this membership. */
  enactedBy: GovActionRef | null;
  /** True after an enacted `NoConfidence`. */
  isDissolved?: boolean;
}

export interface Constitution {
  /** Points at the constitution document; the metadata service resolves it. */
  anchor: Anchor;
  guardrailsScriptHash: Hex | null;
  enactedBy: GovActionRef | null;
  enactedAt: EpochStamp | null;
}

export interface CommitteeApi {
  getCommittee(): Promise<Envelope<Committee>>;
  getMember(coldCredential: string): Promise<Envelope<CommitteeMember>>;
  getConstitution(): Promise<Envelope<Constitution>>;
}
