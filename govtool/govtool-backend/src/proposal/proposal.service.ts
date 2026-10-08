import {
  Inject,
  Injectable,
  InternalServerErrorException,
  NotFoundException,
} from '@nestjs/common';
import type {
  ChainDataApiV1,
  Committee,
  GovActionLineage,
  GovAction,
  GovActionStatus,
} from '@govtool/data-providers/chain-data';

import { CacheService } from 'src/cache/cache.service';
import { asHttp } from 'src/common/errors';
import { assertIdentifier } from 'src/common/hex';
import { legacyGovActionId } from 'src/common/legacy-ids';
import {
  LegacyNetwork,
  epochStartTime,
  type EpochSchedule,
} from 'src/common/legacy-network';
import { compareFiguresDescending, voteFigure } from 'src/common/vote-figures';
import { toLegacyNullableNumber } from 'src/common/legacy';
import { CHAIN_DATA, METADATA } from 'src/providers/providers.module';
import type { MetadataServiceV1 } from '@govtool/data-providers/metadata';
import {
  ENRICH_CONCURRENCY,
  mapLimit,
  proposalDocumentFields,
  proposalFields,
  resolveBody,
  resolveDocument,
} from 'src/metadata/enrich';
import { toLegacyDescription } from 'src/common/legacy-description';
import { DocumentSummaryCache } from 'src/metadata/text-cache';
import { toLegacyParamProposal } from 'src/epoch/epoch.service';
import { readAll } from 'src/common/snapshot';
import {
  EnactedProposalDetailsResponse,
  GetProposalResponse,
  GovernanceActionSortMode,
  GovernanceActionType,
  ListProposalsResponse,
  ProposalResponse,
} from './proposal.type';

/**
 * The contract's action type back to db-sync's spelling. Only one name
 * differs, and every existing client expects db-sync's — so the rename stops
 * at the wire.
 */
const LEGACY_TYPE: Record<GovAction['type'], GovernanceActionType> = {
  ParameterChange: 'ParameterChange',
  HardForkInitiation: 'HardForkInitiation',
  TreasuryWithdrawals: 'TreasuryWithdrawals',
  NoConfidence: 'NoConfidence',
  UpdateCommittee: 'NewCommittee',
  NewConstitution: 'NewConstitution',
  InfoAction: 'InfoAction',
};

/**
 * The legacy endpoint asks by action TYPE; `getEnacted` is keyed by LINEAGE,
 * because `UpdateCommittee` and `NoConfidence` share one and a per-type answer
 * returns a `prevGovActionId` the ledger rejects.
 */
const LINEAGE_OF: Partial<Record<GovernanceActionType, GovActionLineage>> = {
  ParameterChange: 'pparamUpdate',
  HardForkInitiation: 'hardFork',
  NoConfidence: 'committee',
  NewCommittee: 'committee',
  NewConstitution: 'constitution',
};

/**
 * One snapshot row: the legacy proposal, plus the status the list route filters
 * on. The legacy shape has no status field, so it travels alongside.
 */
type ProposalSnapshotEntry = {
  proposal: ProposalResponse;
  status: GovActionStatus;
  /** The contract entity the row was mapped from, for the governanceActions routes. */
  action: GovAction;
};

/**
 * The longest a text search waits for document text it has not cached. The
 * rest is matched from what the cache already holds; the fetches carry on and
 * fill it for the next search. Well inside the frontend's 30 s timeout, and
 * independent of how many actions are searched (a DRep's whole vote history).
 */
const SEARCH_TEXT_WAIT_MS = 5_000;

/**
 * A term that can only be an action id, or a prefix of one: a CIP-129
 * `gov_action1…` id, or a transaction hash (eight or more hex digits) with
 * an optional `#index`. Matched against ids alone, with no metadata wait.
 */
const ACTION_ID_TERM = /^(gov_action1[0-9a-z]*|[0-9a-f]{8,}(#\d*)?)$/i;

/** The CIP-108 strings a search matches, as the Haskell backend's did. */
type ProposalSearchText = Pick<
  ProposalResponse,
  'title' | 'abstract' | 'motivation' | 'rationale'
>;

@Injectable()
export class ProposalService {
  private readonly proposalListSnapshotNamespace = 'proposalListSnapshot';
  /** Search text by (hash, url); see `DocumentSummaryCache`. */
  private readonly searchTextCache =
    new DocumentSummaryCache<ProposalSearchText>();
  private warming = false;

  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    private readonly cacheService: CacheService,
    @Inject(METADATA) private readonly metadata: MetadataServiceV1 | null,
    private readonly network: LegacyNetwork = new LegacyNetwork(chain),
  ) {}

  /**
   * A proposal with what the anchored document gives filled in: its CIP-108
   * text, the document itself as `json` and its authors, as the Haskell
   * backend read them from `off_chain_vote_data`. Every route that sends a
   * proposal to the frontend applies it: the list, the detail and a DRep's
   * vote history, whose rows the details page opens without asking again.
   */
  async withDocument(proposal: ProposalResponse): Promise<ProposalResponse> {
    const fields = proposalDocumentFields(
      await resolveDocument(this.metadata, {
        url: proposal.url,
        hash: proposal.metadataHash,
      }),
    );
    return fields ? { ...proposal, ...fields } : proposal;
  }

  async list(params: {
    type: GovernanceActionType[];
    sort?: GovernanceActionSortMode;
    page: number;
    pageSize: number;
    drepId?: string;
    search?: string;
    /** `txHash#index` ids left out before paging: the DRep's voted actions. */
    excludedIds?: ReadonlySet<string>;
  }): Promise<ListProposalsResponse> {
    return this.cacheService.getOrSet(
      'proposalList',
      {
        type: params.type,
        sort: params.sort,
        page: params.page,
        pageSize: params.pageSize,
        drepId: params.drepId,
        search: params.search,
      },
      () =>
        asHttp(async () => {
          if (params.drepId) {
            assertIdentifier(params.drepId);
          }

          // Live actions only, as the legacy list returned: an action leaves
          // the list once it is ratified, enacted, expired or dropped.
          const proposals = (await this.getProposalSnapshot(''))
            .filter(({ status }) => status === 'live')
            .map(({ proposal }) => proposal)
            .filter(
              ({ txHash, index }) =>
                !params.excludedIds?.has(`${txHash.toLowerCase()}#${index}`),
            );

          let filtered = this.filterByType(proposals, params.type);
          filtered = await this.filterBySearch(filtered, params.search);
          filtered = this.sortProposals(filtered, params.sort);

          const total = filtered.length;
          const start = params.page * params.pageSize;

          return {
            page: params.page,
            pageSize: params.pageSize,
            total,
            elements: await mapLimit(
              filtered.slice(start, start + params.pageSize),
              ENRICH_CONCURRENCY,
              (proposal) => this.withDocument(proposal),
            ),
          };
        }),
    );
  }

  async get(proposalId: string, drepId?: string): Promise<GetProposalResponse> {
    // The frontend sends `txHash#index` (it converts a CIP-129 id to that
    // before calling); either form is accepted, and the provider is asked by
    // the CIP-129 id, the only form it takes.
    const { txHash, index, id } = legacyGovActionId(proposalId);

    // Only charset-checked here: the controller adds the DRep's vote, and a
    // disconnected frontend sends the literal text "undefined".
    if (drepId) {
      assertIdentifier(drepId);
    }

    // Live actions only, as the legacy route answered: an ended action is not
    // found, and the frontend turns that 404 into its history page.
    const proposals = (await this.getProposalSnapshot(id))
      .filter(({ status }) => status === 'live')
      .map(({ proposal }) => proposal);

    if (proposals.length === 0) {
      throw new NotFoundException({
        errorType: 'NotFoundError',
        message: `Proposal with id: ${txHash}#${index} not found`,
      });
    }

    if (proposals.length !== 1) {
      throw new InternalServerErrorException({
        errorType: 'CriticalError',
        message: `Multiple proposals found for id: ${txHash}#${index}. This should never happen`,
      });
    }

    return {
      proposal: await this.withDocument(proposals[0]),
      vote: null,
    };
  }

  /**
   * Every type with a lineage answers with its own (D147); the legacy
   * endpoint answered only `ParameterChange` and `HardForkInitiation` and
   * substituted the hard-fork lineage for any other type. Types without a
   * lineage (`TreasuryWithdrawals`, `InfoAction`) and a missing type keep
   * that substitution, so their responses are unchanged.
   */
  async getEnactedDetails(
    type?: GovernanceActionType,
  ): Promise<EnactedProposalDetailsResponse | null> {
    // `type` is an unvalidated query parameter: only own keys count.
    const lineage =
      (type !== undefined && Object.hasOwn(LINEAGE_OF, type)
        ? LINEAGE_OF[type]
        : undefined) ?? 'hardFork';

    return asHttp(async () => {
      const { data } =
        await this.chain.governance.proposals.getEnacted(lineage);

      if (data === null) {
        return null;
      }

      // `getEnacted` answers with a reference, which is all a transaction
      // needs. The legacy body also carried the action's description, so the
      // action itself is read for it.
      const { data: action } = await this.chain.governance.proposals.get(
        data.id,
      );

      return {
        // Legacy numeric ids: db-sync row ids, which no provider carries now.
        // The legacy shape has no way to say "absent", so this reports null
        // rather than NaN.
        id: null,
        txId: null,
        index: data.index,
        description: action.body,
        hash: data.txHash,
      };
    });
  }

  /** Cached, stale-while-revalidate snapshot — unchanged from the legacy service. */
  /** Every proposal, whatever its status: vote history needs the ended ones. */
  async getProposals(search: string): Promise<ProposalResponse[]> {
    return (await this.getProposalSnapshot(search)).map(
      ({ proposal }) => proposal,
    );
  }

  /**
   * The contract entities behind the snapshot: every action, whatever its
   * status, or with `search` (a CIP-129 id) the one it names. Shares the
   * cached snapshot, so the governance action routes cost no extra provider read.
   */
  async getActions(search = ''): Promise<GovAction[]> {
    return (await this.getProposalSnapshot(search)).map(({ action }) => action);
  }

  private getProposalSnapshot(
    search: string,
  ): Promise<ProposalSnapshotEntry[]> {
    return this.cacheService.getOrSetStaleWhileRevalidate(
      this.proposalListSnapshotNamespace,
      search,
      () => this.fetchProposals(search),
    );
  }

  async warmActiveProposalSnapshot(): Promise<void> {
    await this.cacheService.refresh(
      this.proposalListSnapshotNamespace,
      '',
      () => this.fetchProposals(''),
    );
  }

  private async fetchProposals(
    search: string,
  ): Promise<ProposalSnapshotEntry[]> {
    return asHttp(async () => {
      const elements =
        search === ''
          ? // The full set, paged out of the provider if it caps a page.
            await readAll(
              (page) => this.chain.governance.proposals.list(page),
              {
                label: 'proposal list snapshot',
              },
            )
          : (await this.findOne(search)).data.elements;
      // Only needed to date an epoch the provider reports without a time, so
      // the network is not looked up when every stamp carries one.
      const undated = elements.some(
        ({ lifecycle: { submitted, expires } }) =>
          submitted.time === undefined ||
          (expires !== null && expires.time === undefined),
      );
      const [schedule, committee] = await Promise.all([
        undated ? this.network.epochSchedule() : null,
        this.committeeFor(elements),
      ]);
      return elements.map((action) => ({
        proposal: this.toLegacyProposal(action, schedule, committee),
        status: action.lifecycle.status,
        action,
      }));
    });
  }

  /**
   * The current committee, read only when an UpdateCommittee needs it for
   * each added member's current term. Best effort: only that column needs it.
   */
  private async committeeFor(actions: GovAction[]): Promise<Committee | null> {
    if (!actions.some((a) => a.body.type === 'UpdateCommittee')) return null;
    try {
      return (await this.chain.governance.committee.getCommittee()).data;
    } catch {
      return null;
    }
  }

  /**
   * The exact-id read. The provider reports `NOT_FOUND`; the legacy snapshot
   * contract is "zero or more rows", so it is turned back into an empty page.
   */
  private async findOne(
    legacyId: string,
  ): Promise<{ data: { elements: GovAction[] } }> {
    try {
      const { data } = await this.chain.governance.proposals.get(legacyId);
      return { data: { elements: [data] } };
    } catch (error) {
      if (
        typeof error === 'object' &&
        error !== null &&
        (error as { code?: string }).code === 'NOT_FOUND'
      ) {
        return { data: { elements: [] } };
      }
      throw error;
    }
  }

  /**
   * `GovAction` → the legacy wire shape.
   *
   * Two things must be undone deliberately:
   *  - `type` goes back to db-sync's spelling, so `UpdateCommittee` is
   *    reported as `NewCommittee` the way every existing client expects;
   *  - vote stake comes back as a JSON number, which is lossy above
   *    `Number.MAX_SAFE_INTEGER` but is what the legacy API emitted.
   *
   * `schedule` dates an epoch the provider reports without a time; see
   * `legacyEpochTime`.
   */
  toLegacyProposal(
    action: GovAction,
    schedule: EpochSchedule | null = null,
    committee: Committee | null = null,
  ): ProposalResponse {
    const { submitted, expires } = action.lifecycle;

    return {
      // The legacy `id` was db-sync's internal row id. No provider carries one
      // now, so the canonical CIP-129 action id is used — stable and unique,
      // where `String(undefined)` produced the literal text "undefined".
      id: action.id,
      txHash: action.txHash,
      index: action.index,
      type: LEGACY_TYPE[action.type],
      // db-sync's `description` reshaped by type, as the Haskell backend sent
      // it and the frontend's detail tabs read it.
      details: toLegacyDescription(action, committee),
      expiryDate:
        expires === null
          ? null
          : (expires.time ?? this.legacyEpochTime(schedule, expires.epoch)),
      expiryEpochNo: expires?.epoch ?? null,
      // Falls back to '' only when even the epoch cannot be placed: the
      // legacy field is a required string.
      createdDate:
        submitted.time ?? this.legacyEpochTime(schedule, submitted.epoch) ?? '',
      createdEpochNo: submitted.epoch,
      url: action.anchor?.url ?? null,
      metadataHash: action.anchor?.dataHash ?? null,
      // The legacy `param_proposal` row, in snake_case with every column
      // present: the frontend diffs it key by key against `/epoch/params`.
      protocolParams:
        action.body.type === 'ParameterChange'
          ? toLegacyParamProposal(action.body.changes)
          : null,
      // The four CIP-108 strings, `json` and `authors` come from the anchored
      // document, which chain data never resolves: they are null here and
      // filled through the metadata service when a page is served.
      title: null,
      abstract: null,
      motivation: null,
      rationale: null,
      dRepYesVotes: voteFigure(action, 'drep', 'yes'),
      dRepNoVotes: voteFigure(action, 'drep', 'no'),
      dRepAbstainVotes: voteFigure(action, 'drep', 'abstain'),
      poolYesVotes: voteFigure(action, 'spo', 'yes'),
      poolNoVotes: voteFigure(action, 'spo', 'no'),
      poolAbstainVotes: voteFigure(action, 'spo', 'abstain'),
      ccYesVotes: voteFigure(action, 'cc', 'yes'),
      ccNoVotes: voteFigure(action, 'cc', 'no'),
      ccAbstainVotes: voteFigure(action, 'cc', 'abstain'),
      prevGovActionIndex: toLegacyNullableNumber(
        action.previousAction?.index ?? null,
      ),
      prevGovActionTxHash: action.previousAction?.txHash ?? null,
      json: null,
      authors: [],
    };
  }

  /**
   * The start of `epoch`, for a lifecycle stamp the provider dated by epoch
   * alone (`EpochStamp.time` is optional).
   *
   * - Expiry: exactly what the legacy SQL reported —
   *   `latest_epoch.start_time + (expiration - latest_epoch.no) × epoch
   *   length`, i.e. the start of the expiry epoch (see `EpochSchedule`).
   * - Creation: the legacy value was the submitting block's time, which no
   *   epoch number pins down; the start of the submission epoch is the
   *   closest the stamp allows, and a provider-supplied time always wins.
   *
   * `null` on a network with no known genesis schedule.
   */
  private legacyEpochTime(
    schedule: EpochSchedule | null,
    epoch: number,
  ): string | null {
    return schedule === null ? null : epochStartTime(schedule, epoch);
  }

  filterByType(
    proposals: ProposalResponse[],
    selectedTypes: GovernanceActionType[],
  ): ProposalResponse[] {
    if (selectedTypes.length === 0) {
      return proposals;
    }
    return proposals.filter((proposal) =>
      selectedTypes.includes(proposal.type),
    );
  }

  /**
   * The action id in either form, or the title, abstract, motivation or
   * rationale, as the Haskell backend searched them. The four strings are
   * document text, which the snapshot does not carry, so every candidate's
   * is read through the search text cache before filtering and paging.
   */
  async filterBySearch(
    proposals: ProposalResponse[],
    search?: string,
  ): Promise<ProposalResponse[]> {
    if (!search) {
      return proposals;
    }

    const searchLower = search.toLowerCase();
    const texts = ACTION_ID_TERM.test(search.trim())
      ? new Map<string, ProposalSearchText>()
      : await this.searchTexts(proposals, { waitMs: SEARCH_TEXT_WAIT_MS });

    return proposals.filter((proposal) => {
      const govActionId = `${proposal.txHash}#${proposal.index}`;
      const text = texts.get(proposal.id) ?? proposal;
      const values = [
        govActionId,
        // The CIP-129 action id, which is what `id` carries now and what a
        // provider's own `exactId` search matches.
        proposal.id,
        text.title,
        text.abstract,
        text.motivation,
        text.rationale,
      ].filter((value): value is string => value !== null);

      return values.some((value) => value.toLowerCase().includes(searchLower));
    });
  }

  /**
   * Resolves every live action's search text into the cache, so the first
   * search after a start or a new block does not wait on the documents. One
   * run at a time; failures only leave entries unresolved.
   */
  async warmSearchText(): Promise<void> {
    if (this.metadata === null || this.warming) return;
    this.warming = true;
    try {
      const live = (await this.getProposalSnapshot(''))
        .filter(({ status }) => status === 'live')
        .map(({ proposal }) => proposal);
      await this.searchTexts(live, { awaitRefresh: true });
    } finally {
      this.warming = false;
    }
  }

  /**
   * Each action's search text through the cache. With `waitMs`, waits at most
   * that long in all and then takes whatever the cache holds, so a search
   * does not slow down with the number of uncached documents; the warmer
   * passes `awaitRefresh` and waits for every document instead.
   */
  private async searchTexts(
    proposals: ProposalResponse[],
    options: { awaitRefresh?: boolean; waitMs?: number } = {},
  ): Promise<Map<string, ProposalSearchText>> {
    const out = new Map<string, ProposalSearchText>();
    const metadata = this.metadata;
    if (metadata === null) return out;
    // Never rejects: the cache treats a failed resolution as unresolved.
    const all = mapLimit(proposals, ENRICH_CONCURRENCY, (p) => {
      const { url, metadataHash: hash } = p;
      if (!url || !hash) return Promise.resolve(undefined);
      return this.searchTextCache.get(
        hash,
        url,
        async () => proposalFields(await resolveBody(metadata, { url, hash })),
        { awaitRefresh: options.awaitRefresh },
      );
    });
    let texts: (ProposalSearchText | undefined)[];
    if (options.waitMs === undefined) {
      texts = await all;
    } else {
      let timer: NodeJS.Timeout | undefined;
      const timeout = new Promise<undefined>((resolve) => {
        timer = setTimeout(() => resolve(undefined), options.waitMs);
      });
      texts =
        (await Promise.race([all, timeout]).finally(() =>
          clearTimeout(timer),
        )) ??
        proposals.map(({ url, metadataHash }) =>
          url && metadataHash
            ? this.searchTextCache.peek(metadataHash, url)
            : undefined,
        );
    }
    proposals.forEach((p, i) => {
      const text = texts[i];
      if (text !== undefined) out.set(p.id, text);
    });
    return out;
  }

  sortProposals(
    proposals: ProposalResponse[],
    sort?: GovernanceActionSortMode,
  ): ProposalResponse[] {
    const copied = [...proposals];

    switch (sort) {
      case 'NewestCreated':
        return copied.sort(
          (a, b) => Date.parse(b.createdDate) - Date.parse(a.createdDate),
        );

      case 'SoonestToExpire':
        return copied.sort(
          (a, b) =>
            this.nullableDateSortValue(a.expiryDate) -
            this.nullableDateSortValue(b.expiryDate),
        );

      case 'MostYesVotes':
        return copied.sort((a, b) =>
          compareFiguresDescending(
            this.totalYesVotes(a),
            this.totalYesVotes(b),
          ),
        );

      default:
        return copied;
    }
  }

  /**
   * Summed as bigint, and compared rather than subtracted: three safe-range
   * tallies can add up past Number.MAX_SAFE_INTEGER, and a subtracting
   * comparator on values that large returns 0 for totals that differ, which
   * silently scrambles the sort.
   */
  private totalYesVotes(proposal: ProposalResponse): bigint | null {
    const { dRepYesVotes, poolYesVotes, ccYesVotes } = proposal;
    if (dRepYesVotes === null || poolYesVotes === null || ccYesVotes === null) {
      return null;
    }
    return BigInt(dRepYesVotes) + BigInt(poolYesVotes) + BigInt(ccYesVotes);
  }

  private nullableDateSortValue(value: string | null): number {
    return value === null ? Number.MAX_SAFE_INTEGER : Date.parse(value);
  }
}
