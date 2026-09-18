/**
 * Chain Data API — `/governance/committee` and `/governance/constitution`
 *
 * Constitutional committee membership and the current constitution. Both are
 * read from ledger gov-state where the provider exposes it, rather than
 * reconstructed by replaying enacted `UpdateCommittee` / `NewConstitution`
 * actions — replay is error-prone and gov-state is authoritative.
 */

import type {
  Anchor,
  Envelope,
  EpochNo,
  EpochStamp,
  Hex,
  PagedEnvelope,
  PageRequest,
  Ratio,
} from '../common';
import type { GovActionRef, VoterRef } from '../refs';
import type { ConstitutionBody, MetadataProjection } from '../../metadata';

export interface Credential {
  hash: Hex;
  isScriptBased: boolean;
}

export interface CommitteeMember extends VoterRef {
  role: 'cc';
  coldCredential: Credential;
  hotCredential: Credential | null;
  termStartEpoch: EpochNo | null;
  termExpiryEpoch: EpochNo | null;
  hasResigned: boolean;
}

export interface Committee {
  members: CommitteeMember[];
  quorum: Ratio;
  /** The action that last set this membership. */
  enactedBy: GovActionRef | null;
}

export interface Constitution {
  anchor: Anchor;
  guardrailsScriptHash: Hex | null;
  enactedBy: GovActionRef | null;
  enactedAt: EpochStamp | null;
  /** Resolved document — same lifecycle rules as any other metadata. */
  document: MetadataProjection<ConstitutionBody> | null;
}

export interface CommitteeApi {
  /** `GET /governance/committee` */
  getCommittee(): Promise<Envelope<Committee>>;
  /** `GET /governance/committee/members/{id}` */
  getMember(id: string): Promise<Envelope<CommitteeMember>>;
  /** `GET /governance/constitution` */
  getConstitution(): Promise<Envelope<Constitution>>;
  /** `GET /governance/constitution/history` */
  listConstitutionHistory(
    q?: PageRequest,
  ): Promise<PagedEnvelope<Constitution>>;
}
