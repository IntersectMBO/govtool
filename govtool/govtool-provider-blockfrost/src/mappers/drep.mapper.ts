import type {
  DRep,
  DRepKind,
  DRepDelegator,
  DRepStatus,
  DRepVotingPowerEntry,
  EpochStamp,
  Registration,
  VoterRef,
  VotingPower,
} from '@govtool/data-providers/chain-data';
import type {
  DRepMetadataBody,
  MetadataProjection,
} from '@govtool/data-providers/metadata';

import { encodeCip105DRepId, stripCredentialHeader } from '../common/ids';
import type { BfDRep, BfDRepDelegator, BfDRepMetadata } from '../http/types';
import { projectMetadata } from './metadata.mapper';

/**
 * Blockfrost's `drep_id` is already CIP-129 and its `hex` carries the
 * credential-type header, so both contract ids come straight from the
 * response — `hash` by stripping the header, `cip105Id` by re-encoding.
 */
export function toVoterRef(
  drep: BfDRep | BfDRepMetadata,
  hasScript: boolean,
): VoterRef {
  const hash = stripCredentialHeader(drep.hex);
  return {
    role: 'drep',
    id: drep.drep_id,
    hash,
    isScriptBased: hasScript,
    cip105Id: encodeCip105DRepId(hash, hasScript),
  };
}

/**
 * Blockfrost reports `active`, `retired` and `expired` independently.
 * `retired` is a deregistration and wins; otherwise `active` decides, so an
 * expired DRep (one past its `drep_activity` window) reads as `inactive` —
 * the same three-way split the contract and the legacy API use.
 */
export function deriveStatus(drep: BfDRep): DRepStatus {
  if (drep.retired) return 'retired';
  return drep.active ? 'active' : 'inactive';
}

/**
 * Whether a credential is a DRep proper or a direct ("sole") voter.
 *
 * Blockfrost has no field for this, so it is inferred from whether the
 * credential has a metadata anchor — the same rule the legacy API applied
 * (`url IS NULL` meant a sole voter). It is an inference, not a fact from the
 * source, and it costs the extra `/metadata` read; both are noted in
 * `system.getCapabilities()`.
 */
export function deriveKind(metadata: BfDRepMetadata | null): DRepKind {
  return metadata === null ? 'directVoter' : 'drep';
}

export function drepMetadata(
  metadata: BfDRepMetadata | null,
): MetadataProjection<DRepMetadataBody> | null {
  if (metadata === null) return null;
  return projectMetadata<DRepMetadataBody>({
    url: metadata.url,
    hash: metadata.hash,
    standard: 'CIP119',
    jsonMetadata: metadata.json_metadata,
  });
}

export function mapDRep(input: {
  drep: BfDRep;
  metadata: BfDRepMetadata | null;
  registeredAt: EpochStamp | null;
  lastActiveAt: EpochStamp | null;
}): DRep {
  const { drep, metadata } = input;

  const registration: Registration = {
    status: deriveStatus(drep),
    // Blockfrost exposes no deposit on a DRep record. `null` would claim
    // "no deposit on chain", which is false, so the field is omitted.
    deposit: null,
  };
  if (input.registeredAt !== null) {
    registration.registeredAt = input.registeredAt;
  }

  const result: DRep = {
    ...toVoterRef(drep, drep.has_script),
    role: 'drep',
    kind: deriveKind(metadata),
    registration,
    metadata: drepMetadata(metadata),
    // `amount` is the DRep's stake in the current distribution: an
    // epoch-boundary snapshot, so `active`.
    votingPower: { amount: drep.amount, basis: 'active' },
  };

  if (input.lastActiveAt !== null) {
    result.activity = {
      // Blockfrost gives no vote count, only the last epoch in which the
      // DRep was active. `votesCast` is required, so it is not invented.
      votesCast: 0,
      lastVotedAt: input.lastActiveAt,
    };
  }

  return result;
}

/** `amount` is this delegator's stake as Blockfrost reports it today. */
export function mapDelegator(row: BfDRepDelegator): DRepDelegator {
  return {
    stakeAddress: row.address,
    basis: 'live',
    balance: { total: row.amount },
    since: null,
    txRef: null,
  };
}

export function mapVotingPowerEntry(
  drep: BfDRep,
  metadata: BfDRepMetadata | null,
): DRepVotingPowerEntry {
  const power: VotingPower = { amount: drep.amount, basis: 'active' };
  const entry: DRepVotingPowerEntry = {
    subject: { kind: 'drep', drep: toVoterRef(drep, drep.has_script) },
    votingPower: power,
  };
  const body = drepMetadata(metadata)?.body;
  if (body?.givenName !== undefined) {
    entry.givenName = body.givenName;
  }
  return entry;
}
