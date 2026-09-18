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
  VoteListQuery,
  VotedGovAction,
  VotingPower,
} from '@govtool/data-providers/chain-data';

import { mapWithConcurrency } from '../../common/concurrency';
import type { EpochTimeResolver } from '../../common/epoch-time';
import { notFound, unsupported } from '../../common/errors';
import { toBlockfrostDRepId } from '../../common/ids';
import { envelope } from '../../common/meta';
import { toBlockfrostPage, toContractPage } from '../../common/paging';
import type { BlockfrostClient } from '../../http/client';
import type {
  BfDRep,
  BfDRepDelegator,
  BfDRepMetadata,
  BfDRepRef,
  BfDRepUpdate,
} from '../../http/types';
import {
  mapDRep,
  mapDelegator,
  mapVotingPowerEntry,
} from '../../mappers/drep.mapper';

/** How many entity hydrations run at once; see `mapWithConcurrency`. */
const HYDRATION_CONCURRENCY = 8;

export class BlockfrostDRepsApi implements DRepsApi {
  constructor(
    private readonly client: BlockfrostClient,
    private readonly epochs: EpochTimeResolver,
  ) {}

  /**
   * `/governance/dreps` is a directory of **ids only**, so every element has
   * to be hydrated with its own detail read, plus a metadata read to decide
   * `kind` — two requests per DRep. A page of 25 is 50 requests.
   *
   * That makes the contract's "omit `limit` to get everything" catastrophic
   * here (mainnet has tens of thousands of DReps), so an absent `limit`
   * defaults to one Blockfrost page rather than the whole set.
   *
   * `status`, `kind` and `search` filter the *hydrated page*, not the whole
   * directory: Blockfrost offers no server-side filter, and filtering after
   * paging is the only option that does not mean hydrating everything. A
   * filtered page can therefore come back shorter than `limit` while more
   * matches exist further on — `nextCursor` still advances, so a caller that
   * follows it sees them all. `sort` is refused outright rather than applied
   * to one page and looking like a global ordering.
   */
  async list(q?: DRepListQuery): Promise<PagedEnvelope<DRep>> {
    if (q?.sort !== undefined) {
      throw unsupported(
        'governance.dreps.list{sort}',
        'Blockfrost cannot sort the directory, and sorting one page would misrepresent the order',
      );
    }

    const page = toBlockfrostPage(q, { count: 25 });
    const refs = await this.client.get<BfDRepRef[]>('/governance/dreps', page);

    const hydrated = await mapWithConcurrency(
      refs,
      HYDRATION_CONCURRENCY,
      (ref) => this.hydrate(ref.drep_id),
    );

    let dreps = hydrated.filter((d): d is DRep => d !== null);

    const statuses = q?.status;
    if (statuses !== undefined && statuses.length > 0) {
      dreps = dreps.filter(
        (d) =>
          d.registration.status !== undefined &&
          statuses.includes(d.registration.status),
      );
    }
    const kinds = q?.kind;
    if (kinds !== undefined && kinds.length > 0) {
      dreps = dreps.filter((d) => kinds.includes(d.kind));
    }
    const search = q?.search?.toLowerCase();
    if (search !== undefined && search !== '') {
      dreps = dreps.filter(
        (d) =>
          d.id.toLowerCase() === search ||
          d.hash.toLowerCase() === search ||
          d.cip105Id?.toLowerCase() === search ||
          (d.metadata?.body?.givenName ?? '').toLowerCase().includes(search),
      );
    }

    // The cursor reflects the Blockfrost page that was read, not how many
    // survived filtering, so following it never skips a DRep.
    return envelope({
      ...toContractPage(dreps, page),
      nextCursor: refs.length < page.count ? null : String(page.page + 1),
    });
  }

  async get(
    id: string,
    q?: { expand?: DRepExpand[] },
  ): Promise<Envelope<DRep>> {
    for (const field of q?.expand ?? []) {
      if (field === 'delegators' || field === 'liveVotingPower') {
        throw unsupported(
          `governance.dreps.get#${field}`,
          field === 'delegators'
            ? 'the delegator count means paging the whole delegator list; use listDelegators'
            : 'Blockfrost reports only the epoch-snapshot amount',
        );
      }
    }
    const drep = await this.hydrate(toBlockfrostDRepId(id));
    if (drep === null) {
      throw notFound(`DRep ${id} not found`, { id });
    }
    return envelope(drep);
  }

  async getVotingPower(
    id: string,
    q?: { basis?: StakeBasis; fromEpoch?: EpochNo; toEpoch?: EpochNo },
  ): Promise<Envelope<VotingPower[]>> {
    if (q?.fromEpoch !== undefined || q?.toEpoch !== undefined) {
      throw unsupported(
        'governance.dreps.getVotingPower{fromEpoch,toEpoch}',
        'Blockfrost reports only the current amount, with no history',
      );
    }
    if (q?.basis === 'live') {
      throw unsupported(
        'governance.dreps.getVotingPower{basis=live}',
        'Blockfrost reports the epoch-snapshot amount only',
      );
    }
    const record = await this.client.getOrNull<BfDRep>(
      `/governance/dreps/${toBlockfrostDRepId(id)}`,
    );
    return envelope(
      record === null ? [] : [{ amount: record.amount, basis: 'active' }],
    );
  }

  /**
   * One detail read per id. With no ids this would mean the whole directory —
   * tens of thousands of requests — so it is refused rather than attempted.
   */
  async getVotingPowers(
    ids?: string[],
  ): Promise<Envelope<DRepVotingPowerEntry[]>> {
    if (ids === undefined || ids.length === 0) {
      throw unsupported(
        'governance.dreps.getVotingPowers{all}',
        'without ids this is one request per DRep in the directory; pass explicit ids',
      );
    }

    const entries = await mapWithConcurrency(
      ids,
      HYDRATION_CONCURRENCY,
      async (id) => {
        const bf = toBlockfrostDRepId(id);
        const [drep, metadata] = await Promise.all([
          this.client.getOrNull<BfDRep>(`/governance/dreps/${bf}`),
          this.client.getOrNull<BfDRepMetadata>(
            `/governance/dreps/${bf}/metadata`,
          ),
        ]);
        return drep === null ? null : mapVotingPowerEntry(drep, metadata);
      },
    );

    return envelope(
      entries.filter((e): e is DRepVotingPowerEntry => e !== null),
    );
  }

  /**
   * Not implemented, and this is a data gap rather than a shape gap.
   *
   * `/governance/dreps/{id}/votes` returns `{tx_hash, cert_index, vote}`
   * where the pair locates the **vote** — one transaction carries several at
   * successive indices — and nothing identifies the action voted on. So the
   * choices are known but not what they were about, which is the one thing a
   * voting record has to say.
   *
   * Reconstructing it means listing every proposal and reading each one's
   * votes looking for this voter: on mainnet that is ~1,500 proposals, one
   * request each, per DRep. That is not a read a UI can make, so it is
   * refused rather than offered as a trap.
   */
  listVotes(
    _id: string,
    _q?: VoteListQuery,
  ): Promise<PagedEnvelope<VotedGovAction>> {
    return Promise.reject(
      unsupported(
        'governance.dreps.listVotes',
        '/governance/dreps/{id}/votes does not identify the proposal each vote was cast on',
      ),
    );
  }

  /** Blockfrost gives each delegator's current stake, which db-sync does not. */
  async listDelegators(
    id: string,
    q?: PageRequest & { basis?: StakeBasis; includeBalance?: boolean },
  ): Promise<PagedEnvelope<DRepDelegator>> {
    if (q?.basis === 'active') {
      throw unsupported(
        'governance.dreps.listDelegators{basis=active}',
        'Blockfrost reports each delegator’s live stake, not the epoch snapshot',
      );
    }
    const page = toBlockfrostPage(q, { count: 25 });
    const rows = await this.client.get<BfDRepDelegator[]>(
      `/governance/dreps/${toBlockfrostDRepId(id)}/delegators`,
      page,
    );
    return envelope(toContractPage(rows.map(mapDelegator), page));
  }

  listDelegationEvents(
    _id: string,
    _q?: PageRequest,
  ): Promise<PagedEnvelope<DRepDelegationEvent>> {
    return Promise.reject(
      unsupported(
        'governance.dreps.listDelegationEvents',
        'Blockfrost lists current delegators, with no join or leave history',
      ),
    );
  }

  /**
   * Registration history from `/governance/dreps/{id}/updates`, which names
   * the certificate and its transaction but does not date it and does not
   * carry the anchor — both optional on the contract for exactly this case.
   */
  async listHistory(
    id: string,
    q?: PageRequest,
  ): Promise<PagedEnvelope<DRepHistoryEvent>> {
    const page = toBlockfrostPage(q, { count: 25, order: 'desc' });
    const rows = await this.client.get<BfDRepUpdate[]>(
      `/governance/dreps/${toBlockfrostDRepId(id)}/updates`,
      page,
    );

    const elements: DRepHistoryEvent[] = rows.map((row) => ({
      type:
        row.action === 'deregistered'
          ? ('retired' as const)
          : row.action === 'updated'
            ? ('updated' as const)
            : ('registered' as const),
      txRef: { txHash: row.tx_hash, index: row.cert_index },
    }));

    return envelope(toContractPage(elements, page));
  }

  /**
   * Detail plus metadata. Both are needed for one entity: the detail has the
   * amount and status, and the metadata read is what decides `kind` (see
   * `deriveKind`) as well as filling the projection.
   */
  private async hydrate(blockfrostId: string): Promise<DRep | null> {
    const [drep, metadata] = await Promise.all([
      this.client.getOrNull<BfDRep>(`/governance/dreps/${blockfrostId}`),
      this.client.getOrNull<BfDRepMetadata>(
        `/governance/dreps/${blockfrostId}/metadata`,
      ),
    ]);
    if (drep === null) return null;

    const [registeredAt, lastActiveAt] = await Promise.all([
      this.epochs.stamp(drep.active_epoch),
      this.epochs.stamp(drep.last_active_epoch),
    ]);

    return mapDRep({ drep, metadata, registeredAt, lastActiveAt });
  }
}
