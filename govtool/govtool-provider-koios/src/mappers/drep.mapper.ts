import type {
  DRep,
  DRepDelegator,
  DRepHistoryEvent,
  DRepStatus,
  Registration,
  TxRef,
  VoteRecord,
  VoterRef,
  VotingPower,
} from '@govtool/data-providers/chain-data';
import type { Anchor } from '@govtool/data-providers/metadata';

import {
  encodeCip105DRepId,
  isPredefinedDRep,
  isScriptDRepId,
  toDRepHash,
} from '../common/ids';
import { stripByteaPrefix } from '../common/hex';
import {
  toIsoString,
  toLovelace,
  toNullableLovelace,
  toStrictInteger,
} from '../common/numbers';
import { projectDRepMetadata, projectVoteRationale } from './metadata.mapper';
import type {
  DRepDelegatorRow,
  DRepInfoRow,
  DRepListRow,
  DRepMetadataRow,
  DRepPowerHistoryRow,
  DRepUpdateRow,
  DRepVoteRow,
} from '../rows';

/**
 * Koios' `/drep_list` and `/drep_info` disagree about how registration is
 * reported — the published spec says `drep_status`, the deployment sends a
 * `registered` boolean on `/drep_list` and `drep_status` on `/drep_info` —
 * so both are read and the boolean wins only when the enum is absent.
 */
export function isRegistered(row: DRepListRow | DRepInfoRow): boolean {
  return registrationState(row) === 'registered';
}

/**
 * The three states Koios distinguishes, normalised across both spellings.
 *
 * The distinction between `deregistered` and `not_registered` is load-bearing:
 * a retired DRep *was* registered and has a `retired` status, a history and a
 * credential worth rendering, while a credential that was never registered is
 * a `NOT_FOUND`. Collapsing the two would make `DRepStatus.retired`
 * unreachable from `dreps.get`.
 *
 * Only `/drep_info` sends the enum. `/drep_list`'s boolean has no third state,
 * so `registered: false` there can only be reported as `not_registered`.
 */
export function registrationState(
  row: DRepListRow | DRepInfoRow,
): 'registered' | 'deregistered' | 'not_registered' {
  if (row.drep_status !== undefined) {
    return row.drep_status;
  }
  return row.registered === true ? 'registered' : 'not_registered';
}

/**
 * `active` vs `inactive` is Koios' own `active` flag, which applies the
 * `drepActivity` rule for us — the one derivation db-sync's directory query
 * cannot make, and the reason `dreps.get#registration.status` is supported
 * here and unsupported there.
 */
export function deriveStatus(row: DRepInfoRow): DRepStatus {
  if (registrationState(row) !== 'registered') return 'retired';
  return row.active ? 'active' : 'inactive';
}

export function toVoterRef(row: DRepListRow | DRepInfoRow): VoterRef {
  const hash = row.hex ?? toDRepHash(row.drep_id);
  const isScriptBased = row.has_script || isScriptDRepId(row.drep_id);
  return {
    role: 'drep',
    id: row.drep_id,
    hash,
    isScriptBased,
    cip105Id: encodeCip105DRepId(hash, isScriptBased),
  };
}

/**
 * `drep_info.amount` is the current epoch's row of
 * `drep_voting_power_history` — the epoch-boundary snapshot the ledger counts
 * votes against — so the basis is `active`.
 *
 * The epoch itself is left out: `/drep_info` does not say which epoch the
 * amount belongs to, and the contract would rather have no epoch than a
 * guessed one.
 */
export function mapVotingPower(amount: string | null): VotingPower | null {
  return amount === null
    ? null
    : { amount: toLovelace(amount), basis: 'active' };
}

export function mapPowerHistory(row: DRepPowerHistoryRow): VotingPower {
  return {
    amount: toLovelace(row.amount ?? '0'),
    epoch: toStrictInteger(row.epoch_no),
    basis: 'active',
  };
}

/**
 * The full DRep record.
 *
 * `kind` is always `drep`. GovTool's "direct voter" is a stake credential
 * registered as its own DRep, which db-sync can see because it holds both
 * tables; Koios exposes no way to ask whether a DRep credential is also a
 * stake credential, so the distinction is reported as unsupported rather than
 * guessed — see the `kind` filter on `drep.identity.current` and
 * `DRep.registrationByKind` in `src/capabilities.ts`.
 */
export function mapDRep(
  info: DRepInfoRow,
  options: {
    metadata?: DRepMetadataRow;
    updates?: DRepUpdateRow[];
  } = {},
): DRep {
  const ref = toVoterRef(info);
  const drep: DRep = {
    ...ref,
    role: 'drep',
    kind: 'drep',
    registration: mapRegistration(info, options.updates),
    metadata: mapMetadata(info, options.metadata),
    votingPower: mapVotingPower(info.amount),
  };
  if (info.live_delegator_count !== null) {
    drep.delegators = { live: info.live_delegator_count };
  }
  if (info.expires_epoch_no !== null) {
    drep.activity = {
      votesCast: 0,
      inactiveFromEpoch: info.expires_epoch_no,
    };
  }
  return drep;
}

function mapMetadata(
  info: DRepInfoRow,
  metadata: DRepMetadataRow | undefined,
): DRep['metadata'] {
  return projectDRepMetadata({
    url: metadata?.meta_url ?? info.meta_url,
    hash: metadata?.meta_hash ?? info.meta_hash,
    json: metadata?.meta_json,
    isValid: metadata?.is_valid,
    warning: metadata?.warning,
    comment: metadata?.comment,
  });
}

/**
 * Registration lifecycle.
 *
 * `/drep_info` knows the deposit and the status; only `/drep_updates` knows
 * when the certificates landed, so the timestamps are absent — not `null` —
 * unless the caller's read included them. The contract makes exactly that
 * distinction: absent means "this read did not cover it".
 */
export function mapRegistration(
  info: DRepInfoRow,
  updates?: DRepUpdateRow[],
): Registration {
  const registration: Registration = {
    status: deriveStatus(info),
    deposit: toNullableLovelace(info.deposit),
  };
  if (updates === undefined) {
    return registration;
  }

  const ordered = [...updates].sort((a, b) => b.block_time - a.block_time);
  const registered = ordered.find((row) => row.action === 'registered');
  const retired = ordered.find((row) => row.action === 'deregistered');

  registration.registeredAt =
    registered === undefined
      ? null
      : { time: toIsoString(registered.block_time) };
  registration.registrationTx =
    registered === undefined ? null : toUpdateTxRef(registered);
  registration.retiredAt =
    retired === undefined ? null : { time: toIsoString(retired.block_time) };
  registration.retirementTx =
    retired === undefined ? null : toUpdateTxRef(retired);
  return registration;
}

export function mapHistoryEvent(row: DRepUpdateRow): DRepHistoryEvent {
  return {
    type: row.action === 'deregistered' ? 'retired' : row.action,
    at: { time: toIsoString(row.block_time) },
    txRef: toUpdateTxRef(row),
    anchor: toAnchor(row.meta_url, row.meta_hash),
  };
}

export function toUpdateTxRef(row: DRepUpdateRow): TxRef {
  return {
    txHash: row.update_tx_hash,
    index: row.cert_index,
    at: { time: toIsoString(row.block_time) },
  };
}

function toAnchor(url: string | null, hash: string | null): Anchor | null {
  if (url === null) return null;
  return { url, dataHash: hash === null ? '' : stripByteaPrefix(hash) };
}

/**
 * A delegator row. Koios reports the snapshot epoch and the amount but never
 * the certificate that created the delegation, so `since` and `txRef` are
 * `null`; and the amount is a single total, so the `utxo`/`rewards` split the
 * contract allows for is absent.
 */
export function mapDelegator(row: DRepDelegatorRow): DRepDelegator {
  return {
    stakeAddress: row.stake_address,
    basis: 'active',
    balance: { total: toLovelace(row.amount) },
    since: null,
    txRef: null,
  };
}

/**
 * A vote from `/drep_votes`.
 *
 * That endpoint is scoped to one DRep, so the voter is passed in rather than
 * re-derived per row. `isCurrent` is `true` because the caller has the DRep's
 * complete vote list and a re-vote appears as its own, later row — the
 * supersede pass belongs to whoever paged the set, not to a single row.
 */
export function mapDRepVote(row: DRepVoteRow, voter: VoterRef): VoteRecord {
  const at = { time: toIsoString(row.block_time) };
  return {
    proposal: {
      id: row.proposal_id,
      txHash: row.proposal_tx_hash,
      index: row.proposal_index,
    },
    voter,
    vote: row.vote === 'Yes' ? 'yes' : row.vote === 'No' ? 'no' : 'abstain',
    txRef: { txHash: row.vote_tx_hash, at },
    at,
    votingPower: null,
    rationale: projectVoteRationale({
      url: row.meta_url,
      hash: row.meta_hash,
      json: undefined,
    }),
    isCurrent: true,
  };
}

/** The predefined options are ids Koios accepts but not DRep credentials. */
export function rejectsPredefined(id: string): boolean {
  return isPredefinedDRep(id);
}
