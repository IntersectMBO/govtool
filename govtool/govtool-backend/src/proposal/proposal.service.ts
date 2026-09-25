import {
  Inject,
  Injectable,
  InternalServerErrorException,
  NotFoundException,
} from '@nestjs/common';
import type {
  ChainDataApiV1,
  GovActionLineage,
  GovAction,
  VoteAggregate,
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
import {
  compareIntegers,
  dbInteger,
  type ApiInteger,
} from 'src/common/integer';
import { toLegacyNullableNumber } from 'src/common/legacy';
import { CHAIN_DATA, METADATA } from 'src/providers/providers.module';
import type { MetadataServiceV1 } from '@govtool/data-providers/metadata';
import {
  ENRICH_CONCURRENCY,
  mapLimit,
  proposalFields,
  resolveBody,
} from 'src/metadata/enrich';
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
const LINEAGE_OF: Record<
  'ParameterChange' | 'HardForkInitiation',
  GovActionLineage
> = {
  ParameterChange: 'pparamUpdate',
  HardForkInitiation: 'hardFork',
};

@Injectable()
export class ProposalService {
  private readonly proposalListSnapshotNamespace = 'proposalListSnapshot';

  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    private readonly cacheService: CacheService,
    @Inject(METADATA) private readonly metadata: MetadataServiceV1 | null,
    private readonly network: LegacyNetwork = new LegacyNetwork(chain),
  ) {}

  /** A proposal with its CIP-108 text filled from the anchored document. */
  private async withText(
    proposal: ProposalResponse,
  ): Promise<ProposalResponse> {
    const text = proposalFields(
      await resolveBody(this.metadata, {
        url: proposal.url,
        hash: proposal.metadataHash,
      }),
    );
    return text ? { ...proposal, ...text } : proposal;
  }

  async list(params: {
    type: GovernanceActionType[];
    sort?: GovernanceActionSortMode;
    page: number;
    pageSize: number;
    drepId?: string;
    search?: string;
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

          const proposals = await this.getProposals('');

          let filtered = this.filterByType(proposals, params.type);
          filtered = this.filterBySearch(filtered, params.search);
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
              (proposal) => this.withText(proposal),
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

    // Only charset-checked: it selects nothing yet (`vote` is always null),
    // and a disconnected frontend sends the literal text "undefined".
    if (drepId) {
      assertIdentifier(drepId);
    }

    const proposals = await this.getProposals(id);

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
      proposal: await this.withText(proposals[0]),
      vote: null,
    };
  }

  /**
   * The legacy endpoint substituted `HardForkInitiation` for any type other
   * than itself and `ParameterChange`, because the statement can only return
   * those two. The provider refuses an unanswerable type instead, so the
   * substitution is reproduced here to keep the response identical.
   */
  async getEnactedDetails(
    type?: GovernanceActionType,
  ): Promise<EnactedProposalDetailsResponse | null> {
    const proposalType =
      type === 'ParameterChange' || type === 'HardForkInitiation'
        ? type
        : 'HardForkInitiation';

    return asHttp(async () => {
      const { data } = await this.chain.governance.proposals.getEnacted(
        LINEAGE_OF[proposalType],
      );

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
  async getProposals(search: string): Promise<ProposalResponse[]> {
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

  private async fetchProposals(search: string): Promise<ProposalResponse[]> {
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
      const schedule = undated ? await this.network.epochSchedule() : null;
      return elements.map((action) => this.toLegacyProposal(action, schedule));
    });
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
  ): ProposalResponse {
    const { submitted, expires } = action.lifecycle;
    const aggregates = new Map(
      (action.voteAggregates ?? []).map((aggregate) => [
        aggregate.role,
        aggregate,
      ]),
    );
    const drep = aggregates.get('drep');
    const spo = aggregates.get('spo');
    const cc = aggregates.get('cc');

    return {
      // The legacy `id` was db-sync's internal row id. No provider carries one
      // now, so the canonical CIP-129 action id is used — stable and unique,
      // where `String(undefined)` produced the literal text "undefined".
      id: action.id,
      txHash: action.txHash,
      index: action.index,
      type: LEGACY_TYPE[action.type],
      // Was db-sync's raw `description` column. The typed body is what the
      // action proposes, and there is no untyped fallback any more.
      details: action.body,
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
      // document. Chain data emits the anchor and never resolves it, and this
      // backend has no metadata service wired, so they are absent.
      title: null,
      abstract: null,
      motivation: null,
      rationale: null,
      dRepYesVotes: this.aggregateValue(drep, 'yes'),
      dRepNoVotes: this.aggregateValue(drep, 'no'),
      dRepAbstainVotes: this.aggregateValue(drep, 'abstain'),
      poolYesVotes: this.aggregateValue(spo, 'yes'),
      poolNoVotes: this.aggregateValue(spo, 'no'),
      poolAbstainVotes: this.aggregateValue(spo, 'abstain'),
      ccYesVotes: this.aggregateValue(cc, 'yes'),
      ccNoVotes: this.aggregateValue(cc, 'no'),
      ccAbstainVotes: this.aggregateValue(cc, 'abstain'),
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

  /**
   * One choice off a vote aggregate, as the legacy integer field.
   *
   * The legacy fields are whole numbers — lovelace for the DRep and pool rows,
   * a head count for the committee — so a `percent` aggregate has nothing to
   * put in them. Reporting the fraction rounded to 0 would read as "no votes",
   * so it is refused instead.
   */
  private aggregateValue(
    aggregate: VoteAggregate | undefined,
    choice: 'yes' | 'no' | 'abstain',
  ): ApiInteger {
    if (aggregate === undefined || aggregate.representation === 'percent') {
      return 0;
    }
    return dbInteger(aggregate[choice]);
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

  filterBySearch(
    proposals: ProposalResponse[],
    search?: string,
  ): ProposalResponse[] {
    if (!search) {
      return proposals;
    }

    const searchLower = search.toLowerCase();

    return proposals.filter((proposal) => {
      const govActionId = `${proposal.txHash}#${proposal.index}`;
      const values = [
        govActionId,
        // The CIP-129 action id, which is what `id` carries now and what a
        // provider's own `exactId` search matches.
        proposal.id,
        proposal.title,
        proposal.abstract,
        proposal.motivation,
        proposal.rationale,
      ].filter((value): value is string => value !== null);

      return values.some((value) => value.toLowerCase().includes(searchLower));
    });
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
          compareIntegers(this.totalYesVotes(b), this.totalYesVotes(a)),
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
  private totalYesVotes(proposal: ProposalResponse): bigint {
    return (
      BigInt(proposal.dRepYesVotes) +
      BigInt(proposal.poolYesVotes) +
      BigInt(proposal.ccYesVotes)
    );
  }

  private nullableDateSortValue(value: string | null): number {
    return value === null ? Number.MAX_SAFE_INTEGER : Date.parse(value);
  }
}
