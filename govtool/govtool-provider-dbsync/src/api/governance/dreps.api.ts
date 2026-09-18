import type {
  DRep,
  DRepDelegationEvent,
  DRepDelegator,
  DRepExpand,
  DRepHistoryEvent,
  DRepKind,
  DRepListQuery,
  DRepsApi,
  DRepSort,
  DRepStatus,
  DRepVotingPowerEntry,
  Envelope,
  EpochNo,
  PagedEnvelope,
  PageRequest,
  StakeBasis,
  VoteListQuery,
  VotedGovAction,
  VotingPower,
} from '@govtool/data-providers/chain-data';

import { notFound, unsupported } from '../../common/errors';
import { normalizeDRepId } from '../../common/ids';
import { envelope } from '../../common/meta';
import { paginate } from '../../common/paging';
import { runSql, runSqlArrays } from '../../db/run';
import type { Queryable } from '../../db/queryable';
import {
  mapDRepInfoRow,
  mapDRepListRow,
  mapVotingPowerListRow,
} from '../../mappers/drep.mapper';
import { mapVoteTuple } from '../../mappers/vote.mapper';
import { legacyIdOf } from '../../mappers/proposal.mapper';
import type {
  DRepInfoRow,
  DRepListRow,
  DRepVoteTuple,
  DRepVotingPowerListRow,
  DRepVotingPowerRow,
} from '../../rows';
import type { DbSyncProposalsApi } from './proposals.api';

/** `seededHash` from the legacy service, so `sort: 'random'` orders identically. */
function seededHash(value: string): number {
  let hash = 0;
  for (let index = 0; index < value.length; index += 1) {
    hash = (hash * 31 + value.charCodeAt(index)) | 0;
  }
  return hash;
}

const STATUS_ORDER: Record<DRepStatus, number> = {
  active: 0,
  inactive: 1,
  retired: 2,
};

export class DbSyncDRepsApi implements DRepsApi {
  constructor(
    private readonly db: Queryable,
    private readonly proposals: DbSyncProposalsApi,
  ) {}

  /**
   * `list-dreps.sql` binds the search term six times and once wrapped in
   * `%…%`, and returns the whole directory for that term. Status, kind, sort
   * and paging are applied here, over the materialised rows, exactly as the
   * legacy service did — the statement has no LIMIT and no ORDER BY.
   */
  async list(q?: DRepListQuery): Promise<PagedEnvelope<DRep>> {
    const search = q?.search ?? '';
    const rows = await runSql<DRepListRow>(this.db, 'list-dreps.sql', [
      search,
      search,
      search,
      search,
      search,
      search,
      `%${search}%`,
    ]);

    let dreps = rows.map(mapDRepListRow);

    const statuses = q?.status;
    if (statuses !== undefined && statuses.length > 0) {
      dreps = dreps.filter(
        (drep) =>
          drep.registration.status !== undefined &&
          statuses.includes(drep.registration.status),
      );
    }

    const kinds = q?.kind;
    if (kinds !== undefined && kinds.length > 0) {
      dreps = dreps.filter((drep) => kinds.includes(drep.kind));
    }

    dreps = this.sort(dreps, q?.sort, q?.seed);

    return envelope(paginate(dreps, q));
  }

  /**
   * `get-drep-info.sql` CROSS JOINs single-row CTEs, so it returns exactly
   * one row for any well-formed id — including a credential that was never
   * registered, where every field comes back NULL. The legacy service
   * returned an all-empty record for that; this reports `NOT_FOUND`, which is
   * what the contract requires. A consumer reproducing the legacy body
   * catches it and substitutes the empty record.
   */
  async get(
    id: string,
    q?: { expand?: DRepExpand[] },
  ): Promise<Envelope<DRep>> {
    for (const field of q?.expand ?? []) {
      if (field !== 'metadata') {
        throw unsupported(`governance.dreps.get#${field}`);
      }
    }
    const hash = normalizeDRepId(id);
    const rows = await runSql<DRepInfoRow>(this.db, 'get-drep-info.sql', [
      hash,
    ]);
    const row = rows[0];
    if (row === undefined) {
      throw notFound(`DRep ${id} not found`, { id });
    }
    if (
      row.is_registered_as_drep === null &&
      row.was_registered_as_drep === null &&
      row.is_registered_as_sole_voter === null &&
      row.was_registered_as_sole_voter === null
    ) {
      throw notFound(`DRep ${id} not found`, { id });
    }
    return envelope(mapDRepInfoRow(hash, row));
  }

  /**
   * The latest `drep_distr` amount for one credential. An empty array means
   * no distribution row, which the legacy API reported as 0.
   */
  async getVotingPower(
    id: string,
    q?: { basis?: StakeBasis; fromEpoch?: EpochNo; toEpoch?: EpochNo },
  ): Promise<Envelope<VotingPower[]>> {
    if (q?.fromEpoch !== undefined || q?.toEpoch !== undefined) {
      throw unsupported('governance.dreps.getVotingPower{fromEpoch,toEpoch}');
    }
    if (q?.basis === 'live') {
      throw unsupported('governance.dreps.getVotingPower{basis=live}');
    }
    const hash = normalizeDRepId(id);
    const rows = await runSql<DRepVotingPowerRow>(
      this.db,
      'get-voting-power.sql',
      [hash],
    );
    const row = rows[0];
    if (row === undefined) {
      return envelope([]);
    }
    return envelope([{ amount: String(row.amount), basis: 'active' }]);
  }

  /**
   * Batch read. With no ids this is one statement over the whole directory;
   * with ids it is the filtered statement once per id, which is what the
   * legacy service did (the statement takes a single view/hash pair).
   */
  async getVotingPowers(
    ids?: string[],
  ): Promise<Envelope<DRepVotingPowerEntry[]>> {
    const rows: DRepVotingPowerListRow[] = [];

    if (ids === undefined || ids.length === 0) {
      rows.push(
        ...(await runSql<DRepVotingPowerListRow>(
          this.db,
          'get-dreps-voting-power-list.sql',
        )),
      );
    } else {
      for (const identifier of ids) {
        rows.push(
          ...(await runSql<DRepVotingPowerListRow>(
            this.db,
            'get-filtered-dreps-voting-power.sql',
            [identifier, identifier],
          )),
        );
      }
    }

    return envelope(rows.map(mapVotingPowerListRow));
  }

  /**
   * A DRep's voting record, joined to the action each vote was cast on.
   *
   * `get-votes.sql` is read positionally: its SELECT list has three
   * unaliased `encode(...)` expressions and an unaliased `CONCAT`, so by name
   * the DRep hash is overwritten by the anchor hash and six of nine fields
   * are absent. (The legacy TypeScript service reads it by name and therefore
   * returns nothing; the Haskell backend read it positionally, as here.)
   *
   * Each action is then fetched through the proposals statement, which
   * returns live actions only — so a vote on a concluded action yields no
   * row and is dropped, matching the legacy behaviour.
   */
  async listVotes(
    id: string,
    q?: VoteListQuery,
  ): Promise<PagedEnvelope<VotedGovAction>> {
    for (const field of q?.expand ?? []) {
      if (field !== 'proposal' && field !== 'rationale') {
        throw unsupported(`governance.dreps.listVotes#${field}`);
      }
    }
    const hash = normalizeDRepId(id);
    const tuples = await runSqlArrays<DRepVoteTuple>(this.db, 'get-votes.sql', [
      hash,
    ]);

    const isScript = await this.hasScript(hash);
    const voted: VotedGovAction[] = [];

    for (const tuple of tuples) {
      const vote = mapVoteTuple(tuple, isScript);
      const action = await this.proposals.findLive(legacyIdOf(vote.proposal));
      if (action !== null) {
        voted.push({ vote, proposal: action });
      }
    }

    return envelope(paginate(voted, q));
  }

  listDelegators(
    _id: string,
    _q?: PageRequest & { basis?: StakeBasis; includeBalance?: boolean },
  ): Promise<PagedEnvelope<DRepDelegator>> {
    return Promise.reject(unsupported('governance.dreps.listDelegators'));
  }

  listDelegationEvents(
    _id: string,
    _q?: PageRequest,
  ): Promise<PagedEnvelope<DRepDelegationEvent>> {
    return Promise.reject(unsupported('governance.dreps.listDelegationEvents'));
  }

  listHistory(
    _id: string,
    _q?: PageRequest,
  ): Promise<PagedEnvelope<DRepHistoryEvent>> {
    return Promise.reject(unsupported('governance.dreps.listHistory'));
  }

  /** `get-votes.sql` does not select `has_script`; the profile statement does. */
  private async hasScript(hash: string): Promise<boolean> {
    const rows = await runSql<Pick<DRepInfoRow, 'is_script_based'>>(
      this.db,
      'get-drep-info.sql',
      [hash],
    );
    return rows[0]?.is_script_based ?? false;
  }

  private sort(dreps: DRep[], sort?: DRepSort, seed?: string): DRep[] {
    const copied = [...dreps];
    switch (sort) {
      case 'votingPower':
        return copied.sort((a, b) => this.power(b) - this.power(a));
      case 'activity':
        return copied.sort(
          (a, b) =>
            (b.activity?.votesCast ?? -1) - (a.activity?.votesCast ?? -1),
        );
      case 'registrationDate':
        return copied.sort(
          (a, b) => this.registeredAt(b) - this.registeredAt(a),
        );
      case 'status':
        return copied.sort((a, b) => this.statusRank(a) - this.statusRank(b));
      case 'random':
        return copied.sort(
          (a, b) =>
            seededHash(`${seed ?? ''}:${a.hash}`) -
            seededHash(`${seed ?? ''}:${b.hash}`),
        );
      default:
        return copied;
    }
  }

  /** Legacy ordering used `-1` for absent power, so it sorts below zero. */
  private power(drep: DRep): number {
    return drep.votingPower === null ? -1 : Number(drep.votingPower.amount);
  }

  private registeredAt(drep: DRep): number {
    const time = drep.registration.registeredAt?.time;
    return time === undefined ? 0 : Date.parse(time);
  }

  private statusRank(drep: DRep): number {
    const status = drep.registration.status;
    return status === undefined
      ? Number.MAX_SAFE_INTEGER
      : STATUS_ORDER[status];
  }
}

export type { DRepKind };
