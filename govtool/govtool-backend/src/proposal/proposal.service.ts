import {
  Inject,
  Injectable,
  InternalServerErrorException,
  NotFoundException,
} from '@nestjs/common';
import type {
  ChainDataApiV1,
  GovAction,
} from '@govtool/data-providers/chain-data';
import { toDbSyncType } from '@govtool/provider-dbsync';

import { CacheService } from 'src/cache/cache.service';
import { asHttp } from 'src/common/errors';
import { assertHexText } from 'src/common/hex';
import { compareIntegers, dbInteger } from 'src/common/integer';
import { toLegacyNullableNumber } from 'src/common/legacy';
import { CHAIN_DATA } from 'src/providers/providers.module';
import { readAll } from 'src/common/snapshot';
import {
  EnactedProposalDetailsResponse,
  GetProposalResponse,
  GovernanceActionSortMode,
  GovernanceActionType,
  ListProposalsResponse,
  ProposalResponse,
} from './proposal.type';

@Injectable()
export class ProposalService {
  private readonly proposalListSnapshotNamespace = 'proposalListSnapshot';

  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    private readonly cacheService: CacheService,
  ) {}

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
            assertHexText(params.drepId);
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
            elements: filtered.slice(start, start + params.pageSize),
          };
        }),
    );
  }

  async get(proposalId: string, drepId?: string): Promise<GetProposalResponse> {
    const { txHash, index } = this.parseProposalId(proposalId);

    if (drepId) {
      assertHexText(drepId);
    }

    const proposals = await this.getProposals(`${txHash}#${index}`);

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
      proposal: proposals[0],
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
      const { data } =
        await this.chain.governance.proposals.getEnacted(proposalType);

      if (data === null) {
        return null;
      }

      return {
        // Legacy numeric ids, likewise db-sync-only. `Number(undefined)` is
        // NaN, which serialises to null; that is at least honest about being
        // absent, but it is made explicit here.
        id: this.toLegacyRowId(data.action.providerId),
        txId: this.toLegacyRowId(data.submittedTx?.providerId),
        index: data.action.index,
        description: data.rawBody ?? null,
        hash: data.action.txHash,
      };
    });
  }

  /**
   * A provider-native row id as the legacy numeric field. Providers other
   * than db-sync have none, and the legacy shape has no way to say "absent",
   * so this reports null rather than NaN.
   */
  private toLegacyRowId(providerId: string | undefined): number | null {
    if (providerId === undefined) return null;
    const parsed = Number(providerId);
    return Number.isFinite(parsed) ? parsed : null;
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
      return elements.map((action) => this.toLegacyProposal(action));
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
   */
  toLegacyProposal(action: GovAction): ProposalResponse {
    const body = action.metadata?.body;
    const tallies = new Map(
      (action.tallies ?? []).map((tally) => [tally.role, tally]),
    );
    const drep = tallies.get('drep');
    const spo = tallies.get('spo');
    const cc = tallies.get('cc');

    return {
      // The legacy `id` was db-sync's internal row id, which the contract
      // keeps opaque on `providerId`. A provider that has no such id (Koios,
      // Blockfrost) leaves it unset, so the canonical CIP-129 action id is
      // used instead — stable and unique, where `String(undefined)` produced
      // the literal text "undefined" in the response.
      id: String(action.providerId ?? action.id),
      txHash: action.txHash,
      index: action.index,
      type: toDbSyncType(action.type) as GovernanceActionType,
      details: action.rawBody ?? null,
      expiryDate: action.lifecycle.expires?.time ?? null,
      expiryEpochNo: action.lifecycle.expires?.epoch ?? null,
      // `lifecycle.submitted` is optional on the contract: a provider may know
      // an action's whole lifecycle except when it was submitted. The legacy
      // fields are required, so they fall back rather than dropping out of
      // the response. db-sync always fills the stamp.
      createdDate: action.lifecycle.submitted?.time ?? '',
      createdEpochNo: action.lifecycle.submitted?.epoch ?? 0,
      url: action.metadata?.anchor.url ?? null,
      metadataHash: action.metadata?.anchor.dataHash ?? null,
      protocolParams:
        action.body?.type === 'ParameterChange' ? action.body.changes : null,
      title: body?.title ?? null,
      abstract: body?.abstract ?? null,
      motivation: body?.motivation ?? null,
      rationale: body?.rationale ?? null,
      dRepYesVotes: dbInteger(drep?.stake?.yes ?? 0),
      dRepNoVotes: dbInteger(drep?.stake?.no ?? 0),
      dRepAbstainVotes: dbInteger(drep?.stake?.abstain ?? 0),
      poolYesVotes: dbInteger(spo?.stake?.yes ?? 0),
      poolNoVotes: dbInteger(spo?.stake?.no ?? 0),
      poolAbstainVotes: dbInteger(spo?.stake?.abstain ?? 0),
      ccYesVotes: cc?.count?.yes ?? 0,
      ccNoVotes: cc?.count?.no ?? 0,
      ccAbstainVotes: cc?.count?.abstain ?? 0,
      prevGovActionIndex: toLegacyNullableNumber(
        action.previousAction?.index ?? null,
      ),
      prevGovActionTxHash: action.previousAction?.txHash ?? null,
      json: action.metadata?.raw ?? null,
      // The statement COALESCEs to `[]`, so the legacy field is never null.
      authors: body?.authors ?? [],
    };
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

  private parseProposalId(proposalId: string): {
    txHash: string;
    index: number;
  } {
    const [txHash, rawIndex] = proposalId.split('#');

    if (!txHash || rawIndex === undefined || rawIndex === '') {
      throw new NotFoundException({
        errorType: 'NotFoundError',
        message: `Proposal with id: ${proposalId} not found`,
      });
    }

    assertHexText(txHash);

    const index = Number(rawIndex);

    if (!Number.isInteger(index)) {
      throw new NotFoundException({
        errorType: 'NotFoundError',
        message: `Proposal with id: ${proposalId} not found`,
      });
    }

    return { txHash, index };
  }
}
