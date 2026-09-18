import { Inject, Injectable } from '@nestjs/common';
import type {
  ChainDataApiV1,
  DRep,
  VotedGovAction,
} from '@govtool/data-providers/chain-data';

import { CacheService } from 'src/cache/cache.service';
import { asHttp } from 'src/common/errors';
import { assertHexText } from 'src/common/hex';
import {
  compareIntegers,
  dbInteger,
  type ApiInteger,
} from 'src/common/integer';
import { toLegacyNullableInteger } from 'src/common/legacy';
import { CHAIN_DATA } from 'src/providers/providers.module';
import { readAll } from 'src/common/snapshot';
import { ProposalService } from 'src/proposal/proposal.service';
import {
  GovernanceActionSortMode,
  GovernanceActionType,
  ProposalResponse,
} from 'src/proposal/proposal.type';
import {
  DRepInfoResponse,
  DRepListItem,
  DRepListParams,
  DRepListResponse,
  DRepListSort,
  DRepStatus,
  DRepType,
  DRepVotingPowerListResponse,
  VoteParams,
  VoteResponse,
} from './drep.type';

/** Contract casing → the legacy API's casing. */
const LEGACY_STATUS = {
  active: 'Active',
  inactive: 'Inactive',
  retired: 'Retired',
} as const satisfies Record<string, DRepStatus>;

const LEGACY_KIND = {
  drep: 'DRep',
  directVoter: 'SoleVoter',
} as const satisfies Record<string, DRepType>;

@Injectable()
export class DRepService {
  private readonly drepListSnapshotNamespace = 'drepListSnapshot';

  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    private readonly proposalService: ProposalService,
    private readonly cacheService: CacheService,
  ) {}

  async getVotingPower(drepId: string): Promise<ApiInteger> {
    return this.cacheService.getOrSet('drepVotingPower', drepId, () =>
      asHttp(async () => {
        assertHexText(drepId);
        const { data } =
          await this.chain.governance.dreps.getVotingPower(drepId);
        const first = data[0];
        return first === undefined ? 0 : dbInteger(first.amount);
      }),
    );
  }

  async getVotingPowerList(
    identifiers: string[],
  ): Promise<DRepVotingPowerListResponse[]> {
    return this.cacheService.getOrSet('drepVotingPowerList', identifiers, () =>
      asHttp(async () => {
        const { data } =
          await this.chain.governance.dreps.getVotingPowers(identifiers);

        return data.map((entry) => ({
          // The legacy field is db-sync's pre-CIP-129 `view`; for a
          // predefined option it is the ledger name db-sync stores.
          view:
            entry.subject.kind === 'drep'
              ? (entry.subject.drep.cip105Id ?? entry.subject.drep.id)
              : (entry.subject.view ?? ''),
          // NULL for the predefined options, which have no credential —
          // exactly as the legacy endpoint reported them.
          hashRaw:
            entry.subject.kind === 'drep' ? entry.subject.drep.hash : null,
          votingPower:
            entry.votingPower === null
              ? 0
              : dbInteger(entry.votingPower.amount),
          givenName: entry.givenName ?? null,
        }));
      }),
    );
  }

  /**
   * The legacy endpoint answers with an all-empty record for a credential
   * that was never registered. The provider reports `NOT_FOUND` for that, so
   * it is caught and turned back into the empty record here.
   */
  async getInfo(drepId: string): Promise<DRepInfoResponse> {
    return this.cacheService.getOrSet('drepInfo', drepId, () =>
      asHttp(async () => {
        assertHexText(drepId);

        let drep: DRep;
        try {
          const { data } = await this.chain.governance.dreps.get(drepId);
          drep = data;
        } catch (error) {
          if (
            typeof error === 'object' &&
            error !== null &&
            (error as { code?: string }).code === 'NOT_FOUND'
          ) {
            return this.emptyDRepInfo();
          }
          throw error;
        }

        const byKind = drep.registrationByKind;
        const body = drep.metadata?.body;

        return {
          isScriptBased: drep.isScriptBased,
          isRegisteredAsDRep: byKind?.drep.isRegistered ?? false,
          wasRegisteredAsDRep: byKind?.drep.wasRegistered ?? false,
          isRegisteredAsSoleVoter: byKind?.directVoter.isRegistered ?? false,
          wasRegisteredAsSoleVoter: byKind?.directVoter.wasRegistered ?? false,
          deposit: toLegacyNullableInteger(drep.registration.deposit),
          url: drep.metadata?.anchor.url ?? null,
          dataHash: drep.metadata?.anchor.dataHash ?? null,
          votingPower:
            drep.votingPower === null
              ? null
              : dbInteger(drep.votingPower.amount),
          dRepRegisterTxHash: byKind?.drep.registrationTx?.txHash ?? null,
          dRepRetireTxHash: byKind?.drep.retirementTx?.txHash ?? null,
          soleVoterRegisterTxHash:
            byKind?.directVoter.registrationTx?.txHash ?? null,
          soleVoterRetireTxHash:
            byKind?.directVoter.retirementTx?.txHash ?? null,
          paymentAddress: body?.paymentAddress ?? null,
          givenName: body?.givenName ?? null,
          objectives: body?.objectives ?? null,
          motivations: body?.motivations ?? null,
          qualifications: body?.qualifications ?? null,
          imageUrl: body?.image?.url ?? null,
          imageHash: body?.image?.contentHash ?? null,
        };
      }),
    );
  }

  /**
   * Status filter, the SoleVoter search rule, sorting and paging all run over
   * the cached snapshot, exactly as before — the provider is asked only for
   * the snapshot itself, so one warm cache still serves every filtered view.
   */
  async list(params: DRepListParams): Promise<DRepListResponse> {
    const page = Number(params.page ?? 0);
    const pageSize = Number(params.pageSize ?? 10);
    const search = params.search ?? '';
    const sort = params.sort ?? 'Random';
    const seed = params.seed ?? '';

    let dreps = [...(await this.getDRepListSnapShot(search))];

    if (params.status.length > 0) {
      dreps = dreps.filter((drep) => params.status.includes(drep.status));
    }

    dreps = this.filterDRepsBySearchRule(dreps, search);
    dreps = this.sortDReps(dreps, sort, seed);

    const total = dreps.length;
    const offset = page * pageSize;

    return {
      page,
      pageSize,
      total,
      elements: dreps.slice(offset, offset + pageSize),
    };
  }

  async getVotes(
    drepId: string,
    selectedTypes: GovernanceActionType[] = [],
    sort?: GovernanceActionSortMode,
    search?: string,
  ): Promise<VoteResponse[]> {
    return this.cacheService.getOrSet(
      'drepVotes',
      { drepId, selectedTypes, sort, search },
      () =>
        asHttp(async () => {
          assertHexText(drepId);

          const { data } = await this.chain.governance.dreps.listVotes(drepId);

          const pairs = data.elements.flatMap((element) =>
            this.toLegacyVotePair(element),
          );

          // Filter and sort on the proposal side, as the legacy service did.
          const byGovActionId = new Map(
            pairs.map((pair) => [
              `${pair.proposal.txHash}#${pair.proposal.index}`,
              pair,
            ]),
          );

          let proposals = pairs.map((pair) => pair.proposal);
          proposals = this.proposalService.filterByType(
            proposals,
            selectedTypes,
          );
          proposals = this.proposalService.filterBySearch(proposals, search);
          proposals = this.proposalService.sortProposals(proposals, sort);

          return proposals.flatMap((proposal) => {
            const pair = byGovActionId.get(
              `${proposal.txHash}#${proposal.index}`,
            );
            return pair === undefined ? [] : [{ ...pair, proposal }];
          });
        }),
    );
  }

  async warmDefaultListSnapshot(): Promise<void> {
    await this.cacheService.refresh(
      this.drepListSnapshotNamespace,
      '',
      () => this.fetchDRepListSnapshot(''),
      this.cacheService.drepListTtlSeconds(),
    );
  }

  private getDRepListSnapShot(search: string): Promise<DRepListItem[]> {
    return this.cacheService.getOrSetStaleWhileRevalidate(
      this.drepListSnapshotNamespace,
      search,
      () => this.fetchDRepListSnapshot(search),
      this.cacheService.drepListTtlSeconds(),
    );
  }

  /**
   * The whole directory, paged out of the provider when it caps a page.
   * Everything downstream — the status filter, the SoleVoter rule, the sort
   * and the paging — assumes this snapshot is complete; see `readAll`.
   */
  private async fetchDRepListSnapshot(search: string): Promise<DRepListItem[]> {
    return asHttp(async () => {
      const dreps = await readAll(
        (page) => this.chain.governance.dreps.list({ ...page, search }),
        { label: 'drep list snapshot' },
      );
      return dreps.map((drep) => this.toLegacyListItem(drep));
    });
  }

  private toLegacyListItem(drep: DRep): DRepListItem {
    const body = drep.metadata?.body;
    const status = drep.registration.status;

    return {
      isScriptBased: drep.isScriptBased,
      // The legacy id is the raw hex credential hash, not a bech32 id.
      drepId: drep.hash,
      view: drep.cip105Id ?? drep.id,
      url: drep.metadata?.anchor.url ?? null,
      metadataHash: drep.metadata?.anchor.dataHash ?? null,
      deposit: dbInteger(drep.registration.deposit ?? 0),
      votingPower:
        drep.votingPower === null ? null : dbInteger(drep.votingPower.amount),
      status: status === undefined ? 'Inactive' : LEGACY_STATUS[status],
      type: LEGACY_KIND[drep.kind],
      latestTxHash: drep.registration.registrationTx?.txHash ?? null,
      latestRegistrationDate: drep.registration.registeredAt?.time ?? '',
      metadataError: drep.metadata?.failureMessage ?? null,
      paymentAddress: body?.paymentAddress ?? null,
      givenName: body?.givenName ?? null,
      objectives: body?.objectives ?? null,
      motivations: body?.motivations ?? null,
      qualifications: body?.qualifications ?? null,
      imageUrl: body?.image?.url ?? null,
      imageHash: body?.image?.contentHash ?? null,
      votesLastYear: drep.activity?.votesCast ?? null,
      // `list-dreps.sql` COALESCEs both reference arrays to `[]`, so the
      // legacy field is an array even for a DRep with no metadata anchor —
      // where the contract reports no metadata at all. Never `null`.
      identityReferences: body?.identityReferences ?? [],
      linkReferences: body?.linkReferences ?? [],
    };
  }

  private toLegacyVotePair(
    element: VotedGovAction,
  ): { vote: VoteParams; proposal: ProposalResponse }[] {
    if (element.vote === null) {
      return [];
    }
    const { vote } = element;

    return [
      {
        vote: {
          proposalId: String(vote.proposal.providerId),
          drepId: vote.voter.hash,
          vote: vote.vote,
          url: vote.rationale?.anchor.url ?? null,
          metadataHash: vote.rationale?.anchor.dataHash ?? null,
          // `at` is optional on the contract: a provider may identify a vote
          // by its transaction without dating it. db-sync always dates it,
          // so these fallbacks do not fire on the current wiring.
          epochNo: vote.at?.epoch ?? 0,
          date: vote.at?.time ?? '',
          txHash: vote.txRef.txHash,
        },
        proposal: this.proposalService.toLegacyProposal(element.proposal),
      },
    ];
  }

  private emptyDRepInfo(): DRepInfoResponse {
    return {
      isScriptBased: false,
      isRegisteredAsDRep: false,
      wasRegisteredAsDRep: false,
      isRegisteredAsSoleVoter: false,
      wasRegisteredAsSoleVoter: false,
      deposit: null,
      url: null,
      dataHash: null,
      votingPower: null,
      dRepRegisterTxHash: null,
      dRepRetireTxHash: null,
      soleVoterRegisterTxHash: null,
      soleVoterRetireTxHash: null,
      paymentAddress: null,
      givenName: null,
      objectives: null,
      motivations: null,
      qualifications: null,
      imageUrl: null,
      imageHash: null,
    };
  }

  /**
   * GovTool's directory policy, unchanged: with no search term direct voters
   * are hidden, and with one they appear only on an exact id match. It is a
   * presentation rule rather than a data rule, so it stays in the backend.
   */
  private filterDRepsBySearchRule(
    dreps: DRepListItem[],
    search: string,
  ): DRepListItem[] {
    const searchLower = search.toLowerCase();

    if (searchLower === '') {
      return dreps.filter((drep) => drep.type !== 'SoleVoter');
    }

    return dreps.filter((drep) => {
      if (drep.type !== 'SoleVoter') {
        return true;
      }
      return (
        drep.view.toLowerCase() === searchLower ||
        drep.drepId.toLowerCase() === searchLower
      );
    });
  }

  private sortDReps(
    dreps: DRepListItem[],
    sort?: DRepListSort,
    seed?: string,
  ): DRepListItem[] {
    const copied = [...dreps];

    switch (sort) {
      case 'VotingPower':
        // Compared, not subtracted: a DRep's voting power can exceed the safe
        // range, where subtraction rounds two distinct totals to the same
        // number and the order becomes arbitrary.
        return copied.sort((a, b) =>
          compareIntegers(b.votingPower ?? -1, a.votingPower ?? -1),
        );

      case 'Activity':
        return copied.sort(
          (a, b) => (b.votesLastYear ?? -1) - (a.votesLastYear ?? -1),
        );

      case 'RegistrationDate':
        return copied.sort(
          (a, b) =>
            Date.parse(b.latestRegistrationDate) -
            Date.parse(a.latestRegistrationDate),
        );

      case 'Status':
        return copied.sort(
          (a, b) => this.statusOrder(a.status) - this.statusOrder(b.status),
        );

      case 'Random':
        return copied.sort(
          (a, b) =>
            this.seededHash(`${seed ?? ''}:${a.drepId}`) -
            this.seededHash(`${seed ?? ''}:${b.drepId}`),
        );

      default:
        return copied;
    }
  }

  private statusOrder(status: DRepStatus): number {
    return { Active: 0, Inactive: 1, Retired: 2 }[status];
  }

  private seededHash(value: string): number {
    let hash = 0;
    for (let index = 0; index < value.length; index += 1) {
      hash = (hash * 31 + value.charCodeAt(index)) | 0;
    }
    return hash;
  }
}
