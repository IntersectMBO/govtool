import type {
  DRep,
  DRepKind,
  DRepStatus,
  DRepVotingPowerEntry,
  PredefinedDelegation,
  Registration,
  TxRef,
  VoterRef,
  VotingPower,
} from '@govtool/data-providers/chain-data';
import type {
  DRepMetadataBody,
  MetadataProjection,
  MetadataReference,
} from '@govtool/data-providers/metadata';

import { encodeCip129DRepId } from '../common/ids';
import {
  toInteger,
  toIsoString,
  toLovelace,
  toNullableLovelace,
} from '../common/numbers';
import type { DRepInfoRow, DRepListRow, DRepVotingPowerListRow } from '../rows';
import { definedFields, projectMetadata } from './metadata.mapper';

/* ------------------------------------------------------------------------- */
/* Legacy derivations, ported verbatim from backend-ts DRepService            */
/* ------------------------------------------------------------------------- */

/** `toDRepStatus`: a negative latest deposit is a deregistration. */
export function deriveStatus(active: boolean, deposit: number): DRepStatus {
  if (deposit < 0) {
    return 'retired';
  }
  return active ? 'active' : 'inactive';
}

/** `toDRepType`: the four-way rule GovTool uses to tell a DRep from a direct voter. */
export function deriveKind(
  latestDeposit: number,
  url: string | null,
  hasNonDeregisterVotingAnchor: boolean | null,
): DRepKind {
  if (latestDeposit >= 0 && url === null) {
    return 'directVoter';
  }
  if (latestDeposit >= 0 && url !== null) {
    return 'drep';
  }
  if (latestDeposit < 0 && !hasNonDeregisterVotingAnchor) {
    return 'directVoter';
  }
  return 'drep';
}

/* ------------------------------------------------------------------------- */
/* Shared pieces                                                              */
/* ------------------------------------------------------------------------- */

function voterRef(hash: string, isScript: boolean, view?: string): VoterRef {
  const ref: VoterRef = {
    role: 'drep',
    id: encodeCip129DRepId(hash, isScript),
    hash,
    isScriptBased: isScript,
  };
  if (view !== undefined) {
    ref.cip105Id = view;
  }
  return ref;
}

function txRef(hash: string | null): TxRef | null {
  return hash === null ? null : { txHash: hash };
}

function activePower(amount: string | number | null): VotingPower | null {
  return amount === null
    ? null
    : { amount: toLovelace(amount), basis: 'active' };
}

function drepMetadata(row: {
  url: string | null;
  metadata_hash: string | null;
  fetch_error?: string | null;
  payment_address: string | null;
  given_name: string | null;
  objectives: string | null;
  motivations: string | null;
  qualifications: string | null;
  image_url: string | null;
  image_hash: string | null;
  identity_references?: unknown;
  link_references?: unknown;
}): MetadataProjection<DRepMetadataBody> | null {
  if (row.url === null) {
    return null;
  }

  const { body, hasData } = definedFields<
    Omit<DRepMetadataBody, 'image' | 'identityReferences' | 'linkReferences'>
  >({
    givenName: row.given_name,
    objectives: row.objectives,
    motivations: row.motivations,
    qualifications: row.qualifications,
    paymentAddress: row.payment_address,
  });

  const fullBody: DRepMetadataBody = { ...body };
  let hasBodyData = hasData;

  if (row.image_url !== null || row.image_hash !== null) {
    fullBody.image = {};
    if (row.image_url !== null) {
      fullBody.image.url = row.image_url;
    }
    if (row.image_hash !== null) {
      fullBody.image.contentHash = row.image_hash;
    }
    hasBodyData = true;
  }
  if (
    row.identity_references !== undefined &&
    row.identity_references !== null
  ) {
    fullBody.identityReferences =
      row.identity_references as MetadataReference[];
  }
  if (row.link_references !== undefined && row.link_references !== null) {
    fullBody.linkReferences = row.link_references as MetadataReference[];
  }

  return projectMetadata({
    // voting_anchor.data_hash is NOT NULL in db-sync; the fallback only guards the type.
    anchor: { url: row.url, dataHash: row.metadata_hash ?? '' },
    standard: 'CIP119',
    body: fullBody,
    hasBodyData,
    failureMessage: row.fetch_error ?? null,
  });
}

/* ------------------------------------------------------------------------- */
/* Row → entity                                                               */
/* ------------------------------------------------------------------------- */

/** One directory row (`list-dreps.sql`) → `DRep`. */
export function mapDRepListRow(row: DRepListRow): DRep {
  const deposit = toInteger(row.deposit);
  const latestDeposit = toInteger(row.latest_deposit);

  const registration: Registration = {
    status: deriveStatus(row.active, deposit),
    registeredAt: { time: toIsoString(row.last_register_time) },
    registrationTx: txRef(row.tx_hash),
    // A negative deposit is the legacy signal for a retirement, and it is
    // carried as-is so a consumer can re-derive the legacy field exactly.
    deposit: toLovelace(row.deposit),
  };

  const drep: DRep = {
    ...voterRef(row.drep_hash, row.has_script, row.view),
    role: 'drep',
    kind: deriveKind(
      latestDeposit,
      row.url,
      row.has_non_deregister_voting_anchor,
    ),
    registration,
    metadata: drepMetadata(row),
    votingPower: activePower(row.amount),
  };

  if (row.votes_last_year !== null) {
    drep.activity = { votesCast: toInteger(row.votes_last_year) };
  }

  return drep;
}

/** One profile row (`get-drep-info.sql`) → `DRep`. The row has no view, so `id` is computed. */
export function mapDRepInfoRow(hash: string, row: DRepInfoRow): DRep {
  const isRegisteredAsDRep = row.is_registered_as_drep ?? false;
  const isRegisteredAsDirectVoter = row.is_registered_as_sole_voter ?? false;
  const wasRegisteredAsDRep = row.was_registered_as_drep ?? false;

  let kind: DRepKind;
  if (isRegisteredAsDRep) {
    kind = 'drep';
  } else if (isRegisteredAsDirectVoter) {
    kind = 'directVoter';
  } else {
    kind = wasRegisteredAsDRep ? 'drep' : 'directVoter';
  }

  const byKind: NonNullable<DRep['registrationByKind']> = {
    drep: {
      isRegistered: isRegisteredAsDRep,
      wasRegistered: wasRegisteredAsDRep,
      registrationTx: txRef(row.drep_register_tx_hash),
      retirementTx: txRef(row.drep_retire_tx_hash),
    },
    directVoter: {
      isRegistered: isRegisteredAsDirectVoter,
      wasRegistered: row.was_registered_as_sole_voter ?? false,
      registrationTx: txRef(row.sole_voter_register_tx_hash),
      retirementTx: txRef(row.sole_voter_retire_tx_hash),
    },
  };

  const current = byKind[kind];

  return {
    ...voterRef(hash, row.is_script_based),
    role: 'drep',
    kind,
    registration: {
      registrationTx: current.registrationTx,
      retirementTx: current.retirementTx,
      deposit: toNullableLovelace(row.deposit),
    },
    registrationByKind: byKind,
    metadata: drepMetadata({
      url: row.url,
      metadata_hash: row.data_hash,
      payment_address: row.payment_address,
      given_name: row.given_name,
      objectives: row.objectives,
      motivations: row.motivations,
      qualifications: row.qualifications,
      image_url: row.image_url,
      image_hash: row.image_hash,
    }),
    votingPower: activePower(row.voting_power),
  };
}

/** db-sync's `drep_hash.view` values for the two predefined options. */
const PREDEFINED_BY_VIEW: Record<string, PredefinedDelegation> = {
  drep_always_abstain: 'alwaysAbstain',
  drep_always_no_confidence: 'alwaysNoConfidence',
};

/**
 * One row of either voting-power list statement → entry.
 *
 * Two things this statement does that the directory query does not:
 *
 *  - It includes the predefined options. Their `drep_hash` rows have a NULL
 *    `raw`, so they have no credential hash and no CIP-129 id, and they are
 *    reported as `kind: 'predefined'`. They hold real voting power, so
 *    dropping them would understate the totals.
 *  - It does not select `has_script`; db-sync encodes that in the view's
 *    prefix (`drep_script1…`), which is what is read here.
 */
export function mapVotingPowerListRow(
  row: DRepVotingPowerListRow,
): DRepVotingPowerEntry {
  const votingPower = activePower(row.voting_power);

  if (row.hash_raw === null) {
    const option = PREDEFINED_BY_VIEW[row.view];
    return {
      subject:
        option === undefined
          ? // A NULL raw that is not a known predefined view: report it
            // faithfully rather than guessing which option it is.
            { kind: 'predefined', option: 'alwaysAbstain', view: row.view }
          : { kind: 'predefined', option, view: row.view },
      votingPower,
      givenName: row.given_name,
    };
  }

  return {
    subject: {
      kind: 'drep',
      drep: voterRef(
        row.hash_raw,
        row.view.startsWith('drep_script'),
        row.view,
      ),
    },
    votingPower,
    givenName: row.given_name,
  };
}
