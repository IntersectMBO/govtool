import { BadRequestException, Inject, Injectable } from '@nestjs/common';
import {
  ChainDataError,
  type ChainDataApiV1,
  type DRep,
  type DRepSort,
  type DRepVoteRow,
} from '@govtool/data-providers/chain-data';

import { CacheService } from 'src/cache/cache.service';
import { asHttp, withMethod } from 'src/common/errors';
import {
  decodeCip129DRepId,
  drepIdToCip105,
  drepIdToHex,
  firstFound,
  legacyDRepCandidates,
  tryLegacyDRepCandidates,
} from 'src/common/legacy-ids';
import {
  compareIntegers,
  dbInteger,
  type ApiInteger,
} from 'src/common/integer';
import { toLegacyNullableInteger } from 'src/common/legacy';
import { CHAIN_DATA, METADATA } from 'src/providers/providers.module';
import type { MetadataServiceV1 } from '@govtool/data-providers/metadata';
import {
  drepFields,
  ENRICH_CONCURRENCY,
  mapLimit,
  resolveBody,
} from 'src/metadata/enrich';
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

/**
 * `SoleVoter` was a direct voter — a registration kind the ledger does not
 * have and the contract dropped. `anonymous` is the observable fact that
 * replaced it: a DRep registered with no anchor. The two are not the same set,
 * but they carry the same directory policy — hidden unless the id is typed in
 * full — so the legacy name keeps meaning what the UI does with it.
 */
const LEGACY_KIND = {
  drep: 'DRep',
  anonymous: 'SoleVoter',
} as const satisfies Record<string, DRepType>;

/** The predefined delegation targets, under db-sync's `drep_hash.view`. */
const PREDEFINED_VIEW = {
  drep_always_abstain: 'alwaysAbstainVotingPower',
  drep_always_no_confidence: 'alwaysNoConfidenceVotingPower',
} as const;

/**
 * The legacy endpoint had no cap; each identifier here costs up to two
 * provider reads, so an unbounded list is an amplification vector. The one
 * caller (pdf-ui's poll-voter dialog) asks for at most 1,000.
 */
const MAX_VOTING_POWER_IDENTIFIERS = 1_000;

function isPredefinedView(
  identifier: string,
): identifier is keyof typeof PREDEFINED_VIEW {
  return identifier in PREDEFINED_VIEW;
}

function isNotFound(error: unknown): boolean {
  return (
    typeof error === 'object' &&
    error !== null &&
    (error as { code?: unknown }).code === 'NOT_FOUND'
  );
}

/** Deterministic DRep orderings a snapshot can page through, in preference order. */
const SNAPSHOT_DREP_SORTS: readonly DRepSort[] = [
  'registrationDate',
  'votingPower',
  'activity',
];

@Injectable()
export class DRepService {
  private readonly drepListSnapshotNamespace = 'drepListSnapshot';

  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    private readonly proposalService: ProposalService,
    private readonly cacheService: CacheService,
    @Inject(METADATA) private readonly metadata: MetadataServiceV1 | null,
  ) {}

  /** A DRep's CIP-119 fields from its anchored document, when it resolves. */
  private async profileOf(url: string | null, hash: string | null) {
    return drepFields(await resolveBody(this.metadata, { url, hash }));
  }

  /**
   * The legacy endpoint answered 0 for a credential with no distribution row.
   * Voting power is a field on the DRep now rather than a lookup of its own,
   * so an unknown credential is a `NOT_FOUND` and becomes the same 0.
   */
  async getVotingPower(drepId: string): Promise<ApiInteger> {
    return this.cacheService.getOrSet('drepVotingPower', drepId, () =>
      asHttp(async () => {
        const drep = await this.findDRep(legacyDRepCandidates(drepId));
        return drep?.votingPower === null || drep?.votingPower === undefined
          ? 0
          : dbInteger(drep.votingPower.amount);
      }),
    );
  }

  /**
   * The batch read, one credential at a time.
   *
   * There is no batch method on the contract — voting power travels on the
   * DRep — and the two predefined targets are not DReps at all, so their
   * totals come from the network's stake distribution, which is where the
   * contract puts them.
   */
  async getVotingPowerList(
    identifiers: string[],
  ): Promise<DRepVotingPowerListResponse[]> {
    if (identifiers.length > MAX_VOTING_POWER_IDENTIFIERS) {
      throw new BadRequestException({
        errorType: 'ValidationError',
        message: `At most ${MAX_VOTING_POWER_IDENTIFIERS} identifiers per request`,
      });
    }

    return this.cacheService.getOrSet('drepVotingPowerList', identifiers, () =>
      asHttp(async () => {
        const predefined = identifiers.filter(isPredefinedView);
        const distribution =
          predefined.length === 0
            ? undefined
            : (await this.chain.network.getStakeDistribution()).data;

        const entries = await Promise.all(
          identifiers.map(
            async (identifier): Promise<DRepVotingPowerListResponse[]> => {
              if (isPredefinedView(identifier)) {
                return [
                  {
                    view: identifier,
                    // NULL for the predefined options, which have no credential —
                    // exactly as the legacy endpoint reported them.
                    hashRaw: null,
                    votingPower: dbInteger(
                      distribution?.[PREDEFINED_VIEW[identifier]] ?? 0,
                    ),
                    givenName: null,
                  },
                ];
              }

              // The legacy statement matched each identifier with SQL and
              // simply returned no row for one that matched nothing — a
              // malformed id included. pdf-ui passes whatever DRep ids its
              // comments carry, so one bad id must not fail the whole list: it
              // is dropped here, without ever reaching the provider.
              const candidates = tryLegacyDRepCandidates(identifier);
              const drep =
                candidates === undefined
                  ? null
                  : await this.findDRep(candidates);
              if (drep === null) {
                return [];
              }
              return [
                {
                  // Legacy forms: pdf-ui matches `hashRaw` against the raw hex
                  // DRep id its comments store, and links `view` into the
                  // directory, whose search takes CIP-105.
                  view: drepIdToCip105(drep.id),
                  hashRaw: drepIdToHex(drep.id),
                  votingPower:
                    drep.votingPower == null
                      ? 0
                      : dbInteger(drep.votingPower.amount),
                  // The DRep's CIP-119 name, through the metadata service.
                  givenName:
                    (
                      await this.profileOf(
                        drep.anchor?.url ?? null,
                        drep.anchor?.dataHash ?? null,
                      )
                    )?.givenName ?? null,
                },
              ];
            },
          ),
        );

        return entries.flat();
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
        const drep = await this.findDRep(legacyDRepCandidates(drepId));

        if (drep === null) {
          return this.emptyDRepInfo();
        }

        const profile = await this.profileOf(
          drep.anchor?.url ?? null,
          drep.anchor?.dataHash ?? null,
        );

        // The legacy split, as the Haskell backend drew it: a registration
        // with an anchor is a DRep, one without is a direct ("sole") voter.
        // The "is" flags read the current anchor; the "was" flags read the
        // latest registration certificate, the only one the contract keeps.
        const registered = drep.status !== 'retired';
        const hasAnchor = drep.anchor !== null;
        const latest = drep.registration.latest;
        const registeredWithAnchor =
          latest.anchor === undefined ? hasAnchor : latest.anchor !== null;

        return {
          isScriptBased: drep.isScriptBased ?? false,
          isRegisteredAsDRep: registered && hasAnchor,
          wasRegisteredAsDRep: registeredWithAnchor,
          isRegisteredAsSoleVoter: registered && !hasAnchor,
          wasRegisteredAsSoleVoter: !registeredWithAnchor,
          deposit: toLegacyNullableInteger(drep.registration.latest.deposit),
          url: drep.anchor?.url ?? null,
          dataHash: drep.anchor?.dataHash ?? null,
          votingPower:
            drep.votingPower === null
              ? null
              : dbInteger(drep.votingPower.amount),
          dRepRegisterTxHash: registeredWithAnchor ? latest.txRef.txHash : null,
          // Retirement is dated on the contract but not attributed to a
          // transaction.
          dRepRetireTxHash: null,
          soleVoterRegisterTxHash: registeredWithAnchor
            ? null
            : latest.txRef.txHash,
          soleVoterRetireTxHash: null,
          // The CIP-119 body, resolved through the metadata service; null
          // when it is not configured or the document does not resolve.
          paymentAddress: profile?.paymentAddress ?? null,
          givenName: profile?.givenName ?? null,
          objectives: profile?.objectives ?? null,
          motivations: profile?.motivations ?? null,
          qualifications: profile?.qualifications ?? null,
          imageUrl: profile?.imageUrl ?? null,
          imageHash: profile?.imageHash ?? null,
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

    // Only the page being returned is resolved: the snapshot can hold
    // thousands of DReps, and the metadata service caches each document by
    // hash, so a page someone has looked at before costs nothing.
    const elements = await mapLimit(
      dreps.slice(offset, offset + pageSize),
      ENRICH_CONCURRENCY,
      async (item) => {
        const profile = await this.profileOf(item.url, item.metadataHash);
        return profile ? { ...item, ...profile } : item;
      },
    );

    return { page, pageSize, total, elements };
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
          const candidates = legacyDRepCandidates(drepId);

          const dreps = withMethod(
            this.chain.governance.dreps,
            'listVotes',
            'governance.dreps.listVotes',
          );

          // A bare hash names two possible credentials; which one is
          // registered is settled by reading the DRep, since a vote listing
          // for an unknown DRep is not an error on every provider.
          const id =
            candidates.length === 1
              ? candidates[0]
              : ((await this.findDRep(candidates))?.id ?? null);

          // The legacy statement answered an unknown DRep with no rows.
          if (id === null) {
            return [];
          }

          // The listing carries voted AND not-voted rows, and it is paged like
          // every other collection, so the whole of it is read rather than
          // whichever prefix the first page happened to hold.
          let rows: DRepVoteRow[];
          try {
            rows = await readAll(
              (page) => dreps.listVotes(id, { ...page, voted: true }),
              { label: 'drep vote listing' },
            );
          } catch (error) {
            if (isNotFound(error)) {
              return [];
            }
            throw error;
          }

          // A row names the action it is about; the legacy response carries
          // the whole proposal, which the snapshot already holds.
          const byId = new Map(
            (await this.proposalService.getProposals('')).map((proposal) => [
              proposal.id,
              proposal,
            ]),
          );

          // The legacy vote row named its DRep by the raw hex hash.
          const legacyDRepId = drepIdToHex(id);
          const pairs = rows.flatMap((row) =>
            this.toLegacyVotePair(legacyDRepId, row, byId),
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

  /**
   * The DRep, or `null` where the provider has none of the candidate ids.
   * Candidates come from `legacyDRepCandidates`: one id, or for a bare hash
   * the key-hash id and then the script-hash id.
   */
  private async findDRep(candidates: string[]): Promise<DRep | null> {
    try {
      const { data } = await firstFound(candidates, (id) =>
        this.chain.governance.dreps.get(id),
      );
      return data;
    } catch (error) {
      if (isNotFound(error)) {
        return null;
      }
      throw error;
    }
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
   *
   * `sort` is explicit because the contract's default ordering is RANDOM, and
   * a randomly ordered read is not paged: the provider rejects a second page
   * rather than reshuffling, so asking for the default would cap the snapshot
   * at one page.
   */
  private async fetchDRepListSnapshot(search: string): Promise<DRepListItem[]> {
    return asHttp(async () => {
      const sort = await this.snapshotSort();
      const read = (term: string) =>
        readAll(
          (page) =>
            this.chain.governance.dreps.list({
              ...page,
              sort,
              ...(term === '' ? {} : { search: term }),
            }),
          { label: 'drep list snapshot' },
        );

      // The frontend searches by a legacy id: its search box, the directory
      // detail page and the delegation card all pass the term through
      // dRepSearchPhraseProcessor, which turns any `drep…` id into the raw
      // hex hash. A provider's `exactId` search matches CIP-129 only, so a
      // term that decodes as a DRep id is searched in that form — both forms,
      // for a bare hash — and anything else goes through as free text.
      const candidates =
        search === '' ? undefined : tryLegacyDRepCandidates(search);
      const dreps =
        candidates === undefined
          ? await read(search)
          : (await Promise.all(candidates.map(read))).flat();

      // Only rows with a CIP-129 id. The fixture lists the two predefined
      // targets as DReps under the ids `drep_always_abstain` and
      // `drep_always_no_confidence`; they are not DReps in the contract, had
      // no directory row in the legacy API, and have no hex hash or CIP-105
      // form to report, so they are dropped rather than failing the page.
      const unique = new Map(
        dreps
          .filter((drep) => decodeCip129DRepId(drep.id) !== undefined)
          .map((drep) => [drep.id, drep]),
      );
      return [...unique.values()].map((drep) => this.toLegacyListItem(drep));
    });
  }

  /**
   * The ordering the snapshot is read in. Any deterministic sort will do, since
   * the snapshot is re-sorted in memory, but it must be one the provider
   * declares: an undeclared one is refused, and Blockfrost, for one, declares
   * no `registrationDate` because it costs requests per DRep.
   */
  private async snapshotSort(): Promise<DRepSort> {
    const { data } = await this.chain.system.getCapabilities();
    const sort = SNAPSHOT_DREP_SORTS.find((s) => data.sorts.dreps.includes(s));
    if (sort === undefined) {
      throw new ChainDataError(
        'CAPABILITY_UNSUPPORTED',
        'The provider declares no deterministic DRep ordering, so the directory cannot be read in full',
      );
    }
    return sort;
  }

  private toLegacyListItem(drep: DRep): DRepListItem {
    return {
      isScriptBased: drep.isScriptBased ?? false,
      // The legacy forms, which the frontend depends on: `drepId` is the raw
      // hex hash (compared with the wallet's hex DRep id, and prefixed with a
      // CIP-129 header byte by DRepDetailsCard), `view` the CIP-105 bech32
      // (re-prefixed for script DReps by fixViewForScriptBasedDRep, which
      // would mangle a CIP-129 id).
      drepId: drepIdToHex(drep.id),
      view: drepIdToCip105(drep.id),
      url: drep.anchor?.url ?? null,
      metadataHash: drep.anchor?.dataHash ?? null,
      deposit: dbInteger(drep.registration.latest.deposit ?? 0),
      votingPower:
        drep.votingPower === null ? null : dbInteger(drep.votingPower.amount),
      // A ledger fact now, read off the DRep's expiry epoch rather than
      // reconstructed from its registration.
      status: LEGACY_STATUS[drep.status],
      type: LEGACY_KIND[drep.kind],
      latestTxHash: drep.registration.latest.txRef.txHash,
      latestRegistrationDate: drep.registration.latest.at.time ?? '',
      // Whether an anchor resolved is the metadata service's answer, and this
      // backend has none wired; the same goes for every field below.
      metadataError: null,
      paymentAddress: null,
      givenName: null,
      objectives: null,
      motivations: null,
      qualifications: null,
      imageUrl: null,
      imageHash: null,
      // Participation is "voted out of votable since registration" now, not a
      // trailing year — the legacy field name outlived the window.
      votesLastYear: drep.activity?.voted ?? null,
      // `list-dreps.sql` COALESCEs both reference arrays to `[]`, so the
      // legacy field is an array even for a DRep with no metadata anchor.
      // Never `null`.
      identityReferences: [],
      linkReferences: [],
    };
  }

  private toLegacyVotePair(
    drepId: string,
    row: DRepVoteRow,
    proposals: Map<string, ProposalResponse>,
  ): { vote: VoteParams; proposal: ProposalResponse }[] {
    if (!row.voted) {
      return [];
    }

    const proposal = proposals.get(row.action.id);

    if (proposal === undefined) {
      return [];
    }

    return [
      {
        vote: {
          proposalId: proposal.id,
          drepId,
          vote: row.choice,
          url: row.anchor?.url ?? null,
          metadataHash: row.anchor?.dataHash ?? null,
          // `at` is optional on the contract: a provider may identify a vote
          // by its transaction without dating it.
          epochNo: row.at?.epoch ?? 0,
          date: row.at?.time ?? '',
          txHash: row.txRef.txHash,
        },
        proposal,
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
   * presentation rule rather than a data rule, so it stays in the backend —
   * and it is the same rule the contract states for anonymous DReps.
   */
  private filterDRepsBySearchRule(
    dreps: DRepListItem[],
    search: string,
  ): DRepListItem[] {
    const searchLower = search.toLowerCase();

    if (searchLower === '') {
      return dreps.filter((drep) => drep.type !== 'SoleVoter');
    }

    // An exact id in any accepted form: compared as the raw hash the list
    // item carries, so CIP-129, CIP-105 and hex all reveal the same DRep.
    const searchedHashes = new Set(
      (tryLegacyDRepCandidates(search) ?? []).map(drepIdToHex),
    );

    return dreps.filter((drep) => {
      if (drep.type !== 'SoleVoter') {
        return true;
      }
      return (
        searchedHashes.has(drep.drepId) ||
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
