import type {
  DRep,
  DRepDelegationEvent,
  DRepDelegator,
  DRepExpand,
  DRepHistoryEvent,
  DRepListQuery,
  DRepsApi,
  DRepVotingPowerEntry,
  Envelope,
  EpochNo,
  PagedEnvelope,
  PageRequest,
  StakeBasis,
  VotedGovAction,
  VotingPower,
} from '@govtool/data-providers/chain-data';

import { notFound, unsupported } from '../../common/errors';
import {
  isPredefinedDRep,
  KOIOS_ALWAYS_ABSTAIN,
  normalizeDRepId,
} from '../../common/ids';
import { envelope } from '../../common/meta';
import { chunkByBodySize, toKoiosPage, toPage } from '../../common/paging';
import type { KoiosHttpClient } from '../../http/client';
import {
  mapDelegator,
  mapDRep,
  mapDRepVote,
  mapHistoryEvent,
  mapPowerHistory,
  mapVotingPower,
  registrationState,
  toVoterRef as toDRepVoterRef,
} from '../../mappers/drep.mapper';
import { toVoterRef } from '../../mappers/vote.mapper';
import { mapGovAction } from '../../mappers/proposal.mapper';
import type {
  DRepDelegatorRow,
  DRepInfoRow,
  DRepListRow,
  DRepMetadataRow,
  DRepPowerHistoryRow,
  DRepUpdateRow,
  DRepVoteRow,
  ProposalRow,
} from '../../rows';

/** `/drep_info` and `/drep_metadata` take a POST body of ids; keep it modest. */
/**
 * Koios rejects any POST body over **5,120 bytes** with a 413 — a limit on
 * bytes, not on the number of ids, so a fixed count cannot be right for every
 * endpoint. A CIP-129 DRep id serialises to ~62 bytes, so 100 ids was 6,215
 * bytes and every batched read failed. Chunks are built by measuring instead,
 * with headroom for the JSON envelope and the parameter name.
 */
const MAX_BODY_BYTES = 4096;

export class KoiosDRepsApi implements DRepsApi {
  constructor(private readonly http: KoiosHttpClient) {}

  /**
   * `/drep_list` pages the credentials, then `/drep_info` hydrates the page.
   *
   * Two requests per page, not one per DRep — but the split is also the
   * reason most of `DRepListQuery` is unsupported: `/drep_list` exposes only
   * `drep_id`, `hex`, `has_script` and the registration flag, so there is
   * nothing to sort or search on server-side. Sorting the hydrated *page*
   * would order 1000 arbitrary rows and present it as a ranking, which is
   * worse than refusing.
   */
  async list(q?: DRepListQuery): Promise<PagedEnvelope<DRep>> {
    if (q?.sort !== undefined) {
      throw unsupported(
        'governance.dreps.list{sort}',
        '/drep_list carries no voting power, registration date or activity to sort on',
      );
    }
    // An empty search is "no filter" and is trivially satisfiable — only a
    // real search term is refused. GovTool's backend always passes `search`,
    // defaulting to '', so rejecting on `!== undefined` made the whole DRep
    // directory unreachable through it.
    if (q?.search !== undefined && q.search !== '') {
      throw unsupported(
        'governance.dreps.list{search}',
        'Koios has no index over DRep names or metadata',
      );
    }
    if (q?.kind !== undefined && !q.kind.includes('drep')) {
      throw unsupported(
        'governance.dreps.list{kind}',
        'Koios cannot distinguish a direct voter from a DRep',
      );
    }

    const { offset, limit, page } = toKoiosPage(q);
    const statuses = new Set(q?.status ?? []);
    // `registered` is the only filter `/drep_list` can push down; `active` vs
    // `inactive` needs `/drep_info`, so it is applied after hydration.
    const query =
      statuses.size > 0 && !statuses.has('retired')
        ? { registered: 'eq.true' }
        : {};

    const response = await this.http.get<DRepListRow>('drep_list', query, {
      ...page,
      count: 'estimated',
      order: 'drep_id.asc',
    });

    const expand = new Set<DRepExpand>(q?.expand ?? []);
    const ids = response.rows.map((row) => row.drep_id);
    const [infos, metadata] = await Promise.all([
      this.drepInfo(ids),
      expand.has('metadata') ? this.drepMetadata(ids) : new Map(),
    ]);

    const dreps: DRep[] = [];
    for (const row of response.rows) {
      const info = infos.get(row.drep_id);
      if (info === undefined) continue;
      const drep = mapDRep(info, { metadata: metadata.get(row.drep_id) });
      if (statuses.size > 0 && !statuses.has(drep.registration.status!)) {
        continue;
      }
      dreps.push(drep);
    }

    return envelope(toPage(response, dreps, offset, limit));
  }

  async get(
    id: string,
    q?: { expand?: DRepExpand[] },
  ): Promise<Envelope<DRep>> {
    const drepId = normalizeDRepId(id);
    const expand = new Set<DRepExpand>(q?.expand ?? []);
    if (expand.has('liveVotingPower')) {
      throw unsupported(
        'governance.dreps.get{expand:liveVotingPower}',
        'Koios reports the epoch snapshot only; there is no un-snapshotted DRep power',
      );
    }

    const [infos, metadata, updates] = await Promise.all([
      this.drepInfo([drepId]),
      expand.has('metadata') ? this.drepMetadata([drepId]) : new Map(),
      isPredefinedDRep(drepId) ? Promise.resolve([]) : this.drepUpdates(drepId),
    ]);

    // A retired DRep is returned with `status: 'retired'`; only a credential
    // Koios has never seen a registration certificate for is NOT_FOUND.
    const info = infos.get(drepId);
    if (info === undefined || registrationState(info) === 'not_registered') {
      throw notFound('Koios has no registration for that DRep', { id });
    }

    const drep = mapDRep(info, {
      metadata: metadata.get(drepId),
      updates,
    });
    if (expand.has('activity')) {
      drep.activity = await this.activity(drepId, info.expires_epoch_no);
    }
    return envelope(drep);
  }

  /**
   * `/drep_votes` gives the vote and the action it was cast on, but only as a
   * reference; the contract's `VotedGovAction` carries the whole action, so
   * the page's proposals are fetched in one `/proposal_list` call filtered by
   * `proposal_id=in.(…)`.
   */
  async listVotes(
    id: string,
    q?: PageRequest,
  ): Promise<PagedEnvelope<VotedGovAction>> {
    const drepId = normalizeDRepId(id);
    const { offset, limit, page } = toKoiosPage(q);
    const response = await this.http.get<DRepVoteRow>(
      'drep_votes',
      { _drep_id: drepId },
      { ...page, count: 'estimated', order: 'block_time.desc' },
    );

    const proposals = await this.proposalsById(
      response.rows.map((row) => row.proposal_id),
    );

    const voter = toVoterRef('DRep', drepId);
    const elements: VotedGovAction[] = [];
    for (const row of response.rows) {
      const proposal = proposals.get(row.proposal_id);
      if (proposal === undefined) continue;
      elements.push({
        vote: mapDRepVote(row, voter),
        proposal: mapGovAction(proposal),
      });
    }

    return envelope(toPage(response, elements, offset, limit));
  }

  async listDelegators(
    id: string,
    q?: PageRequest & { basis?: StakeBasis; includeBalance?: boolean },
  ): Promise<PagedEnvelope<DRepDelegator>> {
    if (q?.basis === 'live') {
      throw unsupported(
        'governance.dreps.listDelegators{basis:live}',
        '/drep_delegators reports the epoch snapshot only',
      );
    }
    const { offset, limit, page } = toKoiosPage(q);
    const response = await this.http.get<DRepDelegatorRow>(
      'drep_delegators',
      { _drep_id: normalizeDRepId(id) },
      { ...page, count: 'estimated', order: 'amount.desc' },
    );
    return envelope(
      toPage(response, response.rows.map(mapDelegator), offset, limit),
    );
  }

  listDelegationEvents(
    _id: string,
    _q?: PageRequest,
  ): Promise<PagedEnvelope<DRepDelegationEvent>> {
    return Promise.reject(
      unsupported(
        'governance.dreps.listDelegationEvents',
        'Koios has no per-DRep delegation event stream; /drep_delegators is a snapshot with no join or leave times',
      ),
    );
  }

  /** `/drep_updates` — registration, update and retirement certificates. */
  async listHistory(
    id: string,
    q?: PageRequest,
  ): Promise<PagedEnvelope<DRepHistoryEvent>> {
    const { offset, limit, page } = toKoiosPage(q);
    const response = await this.http.get<DRepUpdateRow>(
      'drep_updates',
      { _drep_id: normalizeDRepId(id) },
      { ...page, count: 'estimated', order: 'block_time.desc' },
    );
    return envelope(
      toPage(response, response.rows.map(mapHistoryEvent), offset, limit),
    );
  }

  /**
   * `/drep_voting_power_history` — a real per-epoch series, which is the
   * other place Koios beats the legacy SQL: db-sync's `get-voting-power.sql`
   * is `ORDER BY epoch_no DESC LIMIT 1` and can only ever answer "now".
   */
  async getVotingPower(
    id: string,
    q?: { basis?: StakeBasis; fromEpoch?: EpochNo; toEpoch?: EpochNo },
  ): Promise<Envelope<VotingPower[]>> {
    if (q?.basis === 'live') {
      throw unsupported(
        'governance.dreps.getVotingPower{basis:live}',
        'Koios reports the epoch snapshot only',
      );
    }
    const drepId = normalizeDRepId(id);

    if (q?.fromEpoch === undefined && q?.toEpoch === undefined) {
      const infos = await this.drepInfo([drepId]);
      const info = infos.get(drepId);
      const power = info === undefined ? null : mapVotingPower(info.amount);
      return envelope(power === null ? [] : [power]);
    }

    const response = await this.http.get<DRepPowerHistoryRow>(
      'drep_voting_power_history',
      { _drep_id: drepId },
      { order: 'epoch_no.desc' },
    );
    const series = response.rows
      .filter(
        (row) =>
          (q.fromEpoch === undefined || row.epoch_no >= q.fromEpoch) &&
          (q.toEpoch === undefined || row.epoch_no <= q.toEpoch),
      )
      .map(mapPowerHistory);
    return envelope(series);
  }

  /**
   * Batch power. With no ids this walks `/drep_list` and hydrates every page,
   * which is many requests — the contract allows it, but a caller asking for
   * "every DRep" on a public Koios tier will meet the rate limiter first.
   */
  async getVotingPowers(
    ids?: string[],
  ): Promise<Envelope<DRepVotingPowerEntry[]>> {
    const requested =
      ids === undefined || ids.length === 0
        ? await this.allDRepIds()
        : ids.map(normalizeDRepId);

    const infos = await this.drepInfo(requested);
    const entries: DRepVotingPowerEntry[] = [];
    for (const id of requested) {
      const info = infos.get(id);
      if (info === undefined) continue;
      entries.push({
        subject: isPredefinedDRep(id)
          ? {
              kind: 'predefined',
              option:
                id === KOIOS_ALWAYS_ABSTAIN
                  ? 'alwaysAbstain'
                  : 'alwaysNoConfidence',
              view: id,
            }
          : { kind: 'drep', drep: toDRepVoterRef(info) },
        votingPower: mapVotingPower(info.amount),
      });
    }
    return envelope(entries);
  }

  /* --------------------------------------------------------------------- */

  /**
   * Vote counts over the DRep's whole history, not a trailing window.
   *
   * db-sync's `list-dreps.sql` counts votes in the trailing 365 days, so the
   * same DRep can report a higher `votesCast` here than there. `notVotedCount`
   * and `participationRate` need the set of actions that were votable while
   * the DRep was registered, which Koios cannot express as a query.
   */
  private async activity(
    drepId: string,
    expiresEpoch: number | null,
  ): Promise<DRep['activity']> {
    const response = await this.http.get<DRepVoteRow>(
      'drep_votes',
      { _drep_id: drepId },
      { limit: 1, count: 'exact', order: 'block_time.desc' },
    );
    const activity: NonNullable<DRep['activity']> = {
      votesCast: response.total ?? response.rows.length,
    };
    const latest = response.rows[0];
    activity.lastVotedAt =
      latest === undefined
        ? null
        : { time: new Date(latest.block_time * 1000).toISOString() };
    if (expiresEpoch !== null) {
      activity.inactiveFromEpoch = expiresEpoch;
    }
    return activity;
  }

  private async allDRepIds(): Promise<string[]> {
    const ids: string[] = [];
    for (let offset = 0; ; offset += 1000) {
      const response = await this.http.get<DRepListRow>(
        'drep_list',
        {},
        { offset, limit: 1000, order: 'drep_id.asc' },
      );
      ids.push(...response.rows.map((row) => row.drep_id));
      if (response.rows.length < 1000) break;
    }
    return ids;
  }

  private async drepInfo(ids: string[]): Promise<Map<string, DRepInfoRow>> {
    return this.batched<DRepInfoRow>('drep_info', ids);
  }

  private async drepMetadata(
    ids: string[],
  ): Promise<Map<string, DRepMetadataRow>> {
    return this.batched<DRepMetadataRow>('drep_metadata', ids);
  }

  /** POST bodies are chunked by size; Koios answers 413 over 5,120 bytes. */
  private async batched<T extends { drep_id: string }>(
    path: string,
    ids: string[],
  ): Promise<Map<string, T>> {
    const out = new Map<string, T>();
    for (const chunk of chunkByBodySize([...new Set(ids)], MAX_BODY_BYTES)) {
      const response = await this.http.post<T>(path, { _drep_ids: chunk });
      for (const row of response.rows) {
        out.set(row.drep_id, row);
      }
    }
    return out;
  }

  private async drepUpdates(drepId: string): Promise<DRepUpdateRow[]> {
    const response = await this.http.get<DRepUpdateRow>(
      'drep_updates',
      { _drep_id: drepId },
      { order: 'block_time.desc' },
    );
    return response.rows;
  }

  private async proposalsById(
    ids: string[],
  ): Promise<Map<string, ProposalRow>> {
    const unique = [...new Set(ids)];
    if (unique.length === 0) return new Map();
    const response = await this.http.get<ProposalRow>('proposal_list', {
      proposal_id: `in.(${unique.join(',')})`,
    });
    return new Map(response.rows.map((row) => [row.proposal_id, row]));
  }
}
