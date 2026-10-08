import { generateKeyPairSync, sign, type KeyObject } from 'node:crypto';
import { HttpException } from '@nestjs/common';
import type {
  ChainDataApiV1,
  Committee,
  Envelope,
  GovAction,
  NetworkInfo,
  OptionalArgument,
  PagedEnvelope,
  ProtocolParams,
  ProviderCapabilities,
  StakeDistribution,
} from '@govtool/data-providers/chain-data';

import type {
  MetadataResult,
  MetadataServiceV1,
} from '@govtool/data-providers/metadata';

import { CacheService } from '../src/cache/cache.service';
import { ConfigService } from '../src/config/config.service';
import { LegacyNetwork } from '../src/common/legacy-network';
import { MetadataService } from '../src/metadata/metadata.service';
import { GovernanceActionsService } from '../src/governance-actions/governance-actions.service';
import { matchesGovernanceActionFilters } from '../src/governance-actions/governance-actions.mapping';
import {
  encodeCborArray,
  hashedBody,
  verifyAuthorWitness,
} from '../src/governance-actions/signature';
import {
  DocumentSummaryCache,
  RESOLVED_TTL_MS,
  UNRESOLVED_MAX_TTL_MS,
  UNRESOLVED_TTL_MS,
} from '../src/metadata/text-cache';
import { ProposalService } from '../src/proposal/proposal.service';
import { SystemService } from '../src/system/system.service';
import { actionId } from './ids';

const META = { provider: 'stub', network: 'preview' } as const;
const env = <T>(data: T): Envelope<T> => ({ data, meta: { ...META } });
const page = <T>(elements: T[]): PagedEnvelope<T> =>
  env({ elements, total: elements.length });

/** Typed per namespace, never cast whole: see legacy-shape.spec.ts. */
type StubApi = {
  network?: Partial<ChainDataApiV1['network']>;
  system?: Partial<ChainDataApiV1['system']>;
  governance?: {
    proposals?: Partial<ChainDataApiV1['governance']['proposals']>;
    committee?: Partial<ChainDataApiV1['governance']['committee']>;
  };
};

function passthroughCache(): CacheService {
  const config = {
    get: () => ({
      cacheDurationSeconds: 0,
      drepListCacheDurationSeconds: 0,
      cacheMaxEntries: 1_000,
    }),
  } as unknown as ConfigService;
  return new CacheService(config);
}

const TX = 'd'.repeat(64);
const TX2 = 'c'.repeat(64);
const COLD_KEY =
  'cc_cold1zgqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq6yewvh';

function govAction(overrides: Partial<GovAction> = {}): GovAction {
  return {
    id: actionId(TX, 0),
    txHash: TX,
    index: 0,
    type: 'InfoAction',
    body: { type: 'InfoAction' },
    lifecycle: {
      status: 'expired',
      submitted: { epoch: 500, time: '2026-01-05T00:00:00Z' },
      submittedTx: { txHash: TX, index: 0 },
      expires: { epoch: 506 },
      ratifiedAt: null,
      enactedAt: null,
      droppedAt: null,
      expiredAt: { epoch: 507 },
    },
    anchor: { url: 'https://x/ga.jsonld', dataHash: 'e'.repeat(64) },
    deposit: '100000000000',
    depositReturnAddress: 'stake1example',
    previousAction: null,
    voteAggregates: [
      {
        role: 'drep',
        representation: 'stake',
        yes: '1000000',
        no: '2000000',
        abstain: '3000000',
        notVoted: '0',
        totalEligible: '6000000',
        threshold: { numerator: 67, denominator: 100 },
      },
      {
        role: 'spo',
        representation: 'stake',
        yes: '4000000',
        no: '0',
        abstain: '0',
        notVoted: '0',
        totalEligible: '4000000',
        threshold: { numerator: 51, denominator: 100 },
      },
      {
        role: 'cc',
        representation: 'count',
        yes: '3',
        no: '1',
        abstain: '0',
        notVoted: '3',
        totalEligible: '7',
        threshold: { numerator: 2, denominator: 3 },
      },
    ],
    ...overrides,
  };
}

const live = govAction({
  id: actionId(TX2, 1),
  txHash: TX2,
  index: 1,
  type: 'TreasuryWithdrawals',
  body: {
    type: 'TreasuryWithdrawals',
    withdrawals: [
      { stakeAddress: 'stake_test1uabc', amount: '9007199254740993' },
    ],
    totalAmount: '9007199254740993',
  },
  lifecycle: {
    status: 'live',
    submitted: { epoch: 505, time: '2026-02-04T00:00:00.000Z' },
    submittedTx: { txHash: TX2, index: 1 },
    expires: { epoch: 511 },
    ratifiedAt: null,
    enactedAt: null,
    droppedAt: null,
    expiredAt: null,
  },
});

const committee: Committee = {
  members: [
    {
      role: 'cc',
      coldCredential: COLD_KEY,
      hotCredential: null,
      termStartEpoch: 500,
      termExpiryEpoch: 520,
      hasResigned: false,
    },
    {
      role: 'cc',
      coldCredential: 'cc_cold1other',
      hotCredential: null,
      termStartEpoch: 500,
      termExpiryEpoch: 505,
      hasResigned: false,
    },
  ],
  quorum: { numerator: 2, denominator: 3 },
  enactedBy: null,
};

const networkInfo: NetworkInfo = {
  network: 'preview',
  era: 'conway',
  tip: { epoch: 510 },
  currentEpoch: 510,
};

const stake: StakeDistribution = {
  epoch: 510,
  totalActiveStake: '100',
  totalStakeControlledByDReps: '9007199254740993',
  totalStakeControlledBySPOs: '800',
  alwaysAbstainVotingPower: '10',
  alwaysNoConfidenceVotingPower: '20',
  spoAlwaysAbstainVotingPower: '30',
  spoAlwaysNoConfidenceVotingPower: '40',
};

function service(
  stub: StubApi,
  options: {
    optionalArguments?: OptionalArgument[];
    pdfApiUrl?: string | null;
    metadata?: Partial<MetadataService>;
    documents?: MetadataServiceV1;
  } = {},
) {
  const api = {
    network: {
      getNetworkInfo: () => Promise.resolve(env(networkInfo)),
      ...stub.network,
    },
    system: stub.system,
    governance: {
      proposals: stub.governance?.proposals ?? {
        list: () => Promise.resolve(page([govAction(), live])),
      },
      committee: stub.governance?.committee ?? {
        getCommittee: () => Promise.resolve(env(committee)),
      },
    },
  } as ChainDataApiV1;
  const cache = passthroughCache();
  const system = {
    getCapabilities: () =>
      Promise.resolve({
        optionalArguments: options.optionalArguments ?? [],
      } as unknown as ProviderCapabilities),
  } as unknown as SystemService;
  const config = {
    get: () => ({ pdfApiUrl: options.pdfApiUrl ?? null }),
  } as unknown as ConfigService;
  return new GovernanceActionsService(
    api,
    options.documents ?? null,
    new ProposalService(api, cache, null, new LegacyNetwork(api)),
    cache,
    new LegacyNetwork(api),
    (options.metadata ?? {}) as MetadataService,
    system,
    config,
  );
}

const LIST = {
  search: '',
  filters: [],
  sort: 'newestFirst' as const,
  page: 1,
  limit: 12,
};

describe('GET /governance-actions', () => {
  it('lists ended actions by default, in the governanceActions UI row shape', async () => {
    const rows = await service({}).list(LIST);
    expect(rows).toEqual([
      {
        id: actionId(TX, 0),
        tx_hash: TX,
        index: 0,
        type: 'InfoAction',
        yes_votes: 1000000,
        no_votes: 2000000,
        abstain_votes: 3000000,
        description: { data: { tag: 'InfoAction' } },
        // Undated stamps are placed on the preview schedule.
        expiry_date: '2024-03-14T00:00:00.000Z',
        expiration: 506,
        time: '2026-01-05T00:00:00.000Z',
        epoch_no: 500,
        url: 'https://x/ga.jsonld',
        data_hash: 'e'.repeat(64),
        proposal_params: null,
        title: null,
        abstract: null,
        status: {
          ratified_epoch: null,
          enacted_epoch: null,
          dropped_epoch: null,
          expired_epoch: 507,
        },
        status_times: {
          ratified_time: null,
          enacted_time: null,
          dropped_time: null,
          expired_time: '2024-03-15T00:00:00.000Z',
        },
      },
    ]);
  });

  it('includes live actions when asked, newest first, withdrawals exact above 2^53', async () => {
    const rows = await service({}).list({
      ...LIST,
      filters: ['live', 'expired'],
    });
    expect(rows.map((r) => r.tx_hash)).toEqual([TX2, TX]);
    expect(rows[0].description).toEqual([
      { receivingAddress: 'stake_test1uabc', amount: 9007199254740993n },
    ]);
    const oldest = await service({}).list({
      ...LIST,
      filters: ['live', 'expired'],
      sort: 'oldestFirst',
    });
    expect(oldest.map((r) => r.tx_hash)).toEqual([TX, TX2]);
  });

  it('pages with 1-based page and limit', async () => {
    const svc = service({});
    const filters = ['live', 'expired'];
    expect(
      (await svc.list({ ...LIST, filters, limit: 1 })).map((r) => r.tx_hash),
    ).toEqual([TX2]);
    expect(
      (await svc.list({ ...LIST, filters, limit: 1, page: 2 })).map(
        (r) => r.tx_hash,
      ),
    ).toEqual([TX]);
    expect(await svc.list({ ...LIST, filters, limit: 1, page: 3 })).toEqual([]);
  });

  it('searches the txHash#index form the UI sends', async () => {
    const rows = await service({}).list({
      ...LIST,
      filters: ['live'],
      search: `${TX2}#1`,
    });
    expect(rows.map((r) => r.tx_hash)).toEqual([TX2]);
  });
});

describe('governanceActions search over document text', () => {
  const HASH_A = 'a'.repeat(64);
  const HASH_B = 'b'.repeat(64);
  const withTitle = govAction({
    anchor: { url: 'https://x/a.jsonld', dataHash: HASH_A },
  });
  const unresolved = govAction({
    id: actionId(TX2, 1),
    txHash: TX2,
    index: 1,
    anchor: { url: 'https://x/b.jsonld', dataHash: HASH_B },
  });

  function documents() {
    const calls: string[] = [];
    const documents = {
      getMetadata: (hash: string): Promise<MetadataResult> => {
        calls.push(hash);
        if (hash !== HASH_A) {
          return Promise.resolve({
            ok: false,
            code: 'HASH_MISMATCH',
            category: 'INVALID_CONTENT',
            message: 'mismatch',
            checkedAt: '2026-01-01T00:00:00Z',
          } as MetadataResult);
        }
        return Promise.resolve({
          ok: true,
          hash,
          fetchedAt: '2026-01-01T00:00:00Z',
          body: {
            body: {
              title: { '@value': 'Treasury Plan' },
              abstract: 'Fund the tooling',
              motivation: 'm',
            },
          },
        });
      },
    } as unknown as MetadataServiceV1;
    return { calls, documents };
  }

  const stub: StubApi = {
    governance: {
      proposals: { list: () => Promise.resolve(page([withTitle, unresolved])) },
    },
  };

  it('matches title and abstract, and resolves each document once across searches', async () => {
    const { calls, documents: docs } = documents();
    const svc = service(stub, { documents: docs });
    const first = await svc.list({ ...LIST, search: 'treasury plan' });
    expect(first.map((r) => [r.id, r.title, r.abstract])).toEqual([
      [withTitle.id, 'Treasury Plan', 'Fund the tooling'],
    ]);
    expect(
      (await svc.list({ ...LIST, search: 'TOOLING' })).map((r) => r.id),
    ).toEqual([withTitle.id]);
    expect(await svc.list({ ...LIST, search: 'nothing' })).toEqual([]);
    // The plain list reads the same cache, and the unresolved document is
    // held for its short TTL rather than asked for on every request.
    const rows = await svc.list(LIST);
    expect(rows.map((r) => r.title)).toEqual(['Treasury Plan', null]);
    expect(calls.sort()).toEqual([HASH_A, HASH_B]);
  });

  it('warms every action so the next search asks for nothing', async () => {
    const { calls, documents: docs } = documents();
    const svc = service(stub, { documents: docs });
    await svc.warmSearchText();
    expect(calls).toHaveLength(2);
    await svc.list({ ...LIST, search: 'treasury' });
    expect(calls).toHaveLength(2);
  });

  it('keeps a resolved summary long, an unresolved one briefly, and bounds the entries', async () => {
    let now = 0;
    const cache = new DocumentSummaryCache(2, () => now);
    let fetches = 0;
    const resolved = () => {
      fetches += 1;
      return Promise.resolve({ title: 't', abstract: null });
    };
    const missing = () => {
      fetches += 1;
      return Promise.resolve(undefined);
    };
    const failing = () => {
      fetches += 1;
      return Promise.reject(new Error('down'));
    };
    // Concurrent callers share one fetch.
    await Promise.all([
      cache.get(HASH_A, 'u', resolved),
      cache.get(HASH_A.toUpperCase(), 'u', resolved),
    ]);
    await expect(cache.get(HASH_B, 'u', missing)).resolves.toBeUndefined();
    expect(fetches).toBe(2);

    // Past the unresolved TTL only the unresolved entry is revalidated.
    now = UNRESOLVED_TTL_MS + 1;
    await cache.get(HASH_A, 'u', resolved);
    await cache.get(HASH_B, 'u', missing);
    expect(fetches).toBe(3);

    // Past the resolved TTL too; a failing refresh keeps the summary.
    now = RESOLVED_TTL_MS + 1;
    await expect(cache.get(HASH_A, 'u', failing)).resolves.toEqual({
      title: 't',
      abstract: null,
    });
    expect(fetches).toBe(4);

    await cache.get('c'.repeat(64), 'u', resolved);
    await cache.get('d'.repeat(64), 'u', resolved);
    expect(cache.size).toBe(2);
  });
});

describe('DocumentSummaryCache stale-while-revalidate', () => {
  const HASH_A = 'a'.repeat(64);
  const HASH_B = 'b'.repeat(64);
  const T1 = { title: 'one', abstract: null };
  const T2 = { title: 'two', abstract: 'b' };
  const deferred = <T>() => {
    let resolve!: (value: T) => void;
    let reject!: (error: unknown) => void;
    const promise = new Promise<T>((res, rej) => {
      resolve = res;
      reject = rej;
    });
    return { promise, resolve, reject };
  };
  const flush = () => new Promise((r) => setImmediate(r));

  it('awaits the first fetch', async () => {
    const cache = new DocumentSummaryCache(10, () => 0);
    const first = deferred<typeof T1>();
    let settled = false;
    const got = cache
      .get(HASH_A, 'u', () => first.promise)
      .then((v) => {
        settled = true;
        return v;
      });
    await flush();
    expect(settled).toBe(false);
    first.resolve(T1);
    await expect(got).resolves.toEqual(T1);
  });

  it('serves an expired entry at once and refreshes it once in the background', async () => {
    let now = 0;
    const cache = new DocumentSummaryCache(10, () => now);
    await cache.get(HASH_A, 'u', () => Promise.resolve(T1));
    now = RESOLVED_TTL_MS + 1;
    const refresh = deferred<typeof T2>();
    let fetches = 0;
    const slow = () => {
      fetches += 1;
      return refresh.promise;
    };
    // Both callers get the old value while the refresh is still pending.
    await expect(cache.get(HASH_A, 'u', slow)).resolves.toEqual(T1);
    await expect(cache.get(HASH_A, 'u', slow)).resolves.toEqual(T1);
    expect(fetches).toBe(1);
    refresh.resolve(T2);
    await flush();
    await expect(cache.get(HASH_A, 'u', slow)).resolves.toEqual(T2);
    expect(fetches).toBe(1);
  });

  it('serves an expired unresolved entry at once too', async () => {
    let now = 0;
    const cache = new DocumentSummaryCache(10, () => now);
    await cache.get(HASH_B, 'u', () => Promise.resolve(undefined));
    now = UNRESOLVED_TTL_MS + 1;
    const never = deferred<typeof T1>();
    await expect(
      cache.get(HASH_B, 'u', () => never.promise),
    ).resolves.toBeUndefined();
  });

  it('keeps the value when a refresh fails or comes back unresolved', async () => {
    let now = 0;
    const cache = new DocumentSummaryCache(10, () => now);
    await cache.get(HASH_A, 'u', () => Promise.resolve(T1));
    now = RESOLVED_TTL_MS + 1;
    await cache.get(HASH_A, 'u', () => Promise.reject(new Error('down')));
    await flush();
    await expect(
      cache.get(HASH_A, 'u', () => Promise.resolve(T2)),
    ).resolves.toEqual(T1);
    // The failure is retried after the short TTL, not the long one.
    now += UNRESOLVED_TTL_MS + 1;
    await cache.get(HASH_A, 'u', () => Promise.resolve(undefined));
    await flush();
    await expect(
      cache.get(HASH_A, 'u', () => Promise.resolve(T2)),
    ).resolves.toEqual(T1);
  });

  it('backs off a key that stays unresolved, up to the cap, and resets once it resolves', async () => {
    let now = 0;
    // 0.5 leaves each backed-off wait unspread.
    const cache = new DocumentSummaryCache(
      10,
      () => now,
      8,
      () => 0.5,
    );
    let fetches = 0;
    const missing = () => {
      fetches += 1;
      return Promise.resolve(undefined);
    };
    const warm = (resolve: () => Promise<typeof T1 | undefined>) =>
      cache.get(HASH_B, 'u', resolve, { awaitRefresh: true });
    await warm(missing);
    expect(fetches).toBe(1);

    // Each unresolved answer doubles the wait: 5, 10, 20, 40, then 60 min.
    const waits: number[] = [];
    let last = now;
    while (waits.length < 6) {
      now += 60_000;
      await warm(missing);
      if (fetches > waits.length + 1) {
        waits.push(now - last);
        last = now;
      }
    }
    const minute = 60_000;
    expect(waits).toEqual([5, 10, 20, 40, 60, 60].map((m) => m * minute));
    expect(UNRESOLVED_MAX_TTL_MS).toBe(60 * minute);

    // A resolved answer ends the backoff; the next miss starts over.
    now += UNRESOLVED_MAX_TTL_MS;
    await warm(() => Promise.resolve(T1));
    now += RESOLVED_TTL_MS + 1;
    await warm(missing);
    const before = fetches;
    now += UNRESOLVED_TTL_MS + 1;
    await warm(missing);
    expect(fetches).toBe(before + 1);
  });

  it('spreads backed-off retries so keys that failed together come due apart', async () => {
    let now = 0;
    const draws = [0, 0.999];
    let i = 0;
    const cache = new DocumentSummaryCache(
      10,
      () => now,
      8,
      () => draws[i++ % draws.length],
    );
    const fetched: string[] = [];
    const missing = (key: string) => () => {
      fetched.push(key);
      return Promise.resolve(undefined);
    };
    const warm = (key: string) =>
      cache.get(key, 'u', missing(key), { awaitRefresh: true });
    await warm(HASH_A);
    await warm(HASH_B);
    // The first retry is not spread: both come due at 5 minutes.
    now = UNRESOLVED_TTL_MS;
    await warm(HASH_A);
    await warm(HASH_B);
    expect(fetched).toEqual([HASH_A, HASH_B, HASH_A, HASH_B]);
    // The second wait is 10 minutes, drawn to 7.5 for A and about 12.5 for B.
    now = UNRESOLVED_TTL_MS + 8 * 60_000;
    await warm(HASH_A);
    await warm(HASH_B);
    expect(fetched).toEqual([HASH_A, HASH_B, HASH_A, HASH_B, HASH_A]);
  });

  it('lets the warmer await a refresh that a search does not', async () => {
    let now = 0;
    const cache = new DocumentSummaryCache(10, () => now);
    await cache.get(HASH_A, 'u', () => Promise.resolve(T1));
    now = RESOLVED_TTL_MS + 1;
    const refresh = deferred<typeof T2>();
    const warm = cache.get(HASH_A, 'u', () => refresh.promise, {
      awaitRefresh: true,
    });
    await expect(
      cache.get(HASH_A, 'u', () => refresh.promise),
    ).resolves.toEqual(T1);
    refresh.resolve(T2);
    await expect(warm).resolves.toEqual(T2);
  });

  it('bounds concurrent background refreshes', async () => {
    let now = 0;
    const cache = new DocumentSummaryCache(10, () => now, 2);
    const keys = ['a', 'b', 'c', 'd'].map((c) => c.repeat(64));
    for (const k of keys) await cache.get(k, 'u', () => Promise.resolve(T1));
    now = RESOLVED_TTL_MS + 1;
    let running = 0;
    let peak = 0;
    const pending: Array<() => void> = [];
    const slow = () => {
      running += 1;
      peak = Math.max(peak, running);
      return new Promise<typeof T2>((res) =>
        pending.push(() => {
          running -= 1;
          res(T2);
        }),
      );
    };
    for (const k of keys) await cache.get(k, 'u', slow);
    await flush();
    expect(pending).toHaveLength(2);
    while (pending.length > 0) {
      pending.shift()!();
      await flush();
    }
    expect(peak).toBe(2);
    for (const k of keys) {
      await expect(cache.get(k, 'u', slow)).resolves.toEqual(T2);
    }
  });
});

describe('list filter semantics', () => {
  const ended = govAction();
  it.each([
    [[], false, true],
    [['live'], true, false],
    [['InfoAction'], false, true],
    [['TreasuryWithdrawals'], false, false],
    [['TreasuryWithdrawals', 'live'], true, false],
    [['expired'], false, true],
    [['ratified'], false, false],
  ])('%j → live %s, expired InfoAction %s', (filters, liveIn, endedIn) => {
    expect(matchesGovernanceActionFilters(live, filters)).toBe(liveIn);
    expect(matchesGovernanceActionFilters(ended, filters)).toBe(endedIn);
  });
});

describe('GET /governance-actions/:id', () => {
  const update = govAction({
    type: 'UpdateCommittee',
    body: {
      type: 'UpdateCommittee',
      added: [{ coldCredential: COLD_KEY, termExpiryEpoch: 600 }],
      removed: [],
      quorum: { numerator: 2, denominator: 3 },
    },
    previousAction: { id: actionId(TX2, 1), txHash: TX2, index: 1 },
  });

  it('answers the detail row, committee change enriched with the current term', async () => {
    const svc = service({
      governance: { proposals: { get: () => Promise.resolve(env(update)) } },
    });
    const row = await svc.get(TX, '0');
    expect(Object.keys(row)).toEqual([
      'id',
      'tx_hash',
      'index',
      'type',
      'description',
      'expiry_date',
      'expiration',
      'time',
      'epoch_no',
      'url',
      'data_hash',
      'proposal_params',
      'json_metadata',
      'title',
      'abstract',
      'motivation',
      'rationale',
      'yes_votes',
      'no_votes',
      'abstain_votes',
      'pool_yes_votes',
      'pool_no_votes',
      'pool_abstain_votes',
      'cc_yes_votes',
      'cc_no_votes',
      'cc_abstain_votes',
      'vote_aggregates',
      'prev_gov_action_index',
      'prev_gov_action_tx_hash',
      'used_epoch_no',
      'status',
      'status_times',
    ]);
    expect(row).toMatchObject({
      type: 'NewCommittee',
      description: {
        tag: 'UpdateCommittee',
        members: [
          {
            hash: '0'.repeat(56),
            type: 'keyHash',
            expirationEpoch: 520,
            hasScript: false,
            newExpirationEpoch: 600,
          },
        ],
        membersToBeRemoved: [],
        threshold: 2 / 3,
      },
      pool_yes_votes: 4000000,
      cc_yes_votes: 3,
      cc_no_votes: 1,
      prev_gov_action_index: '1',
      prev_gov_action_tx_hash: TX2,
      used_epoch_no: 507,
    });
  });

  it('keeps supported historical tallies when another role and network metrics are unavailable', async () => {
    const action = govAction();
    const aggregates = action.voteAggregates!.filter((a) => a.role !== 'cc');
    const networkMetrics = jest.fn(() =>
      Promise.reject(new Error('unsupported metrics')),
    );
    const svc = service({
      network: { getStakeDistribution: networkMetrics },
      governance: {
        proposals: {
          get: () =>
            Promise.resolve(env({ ...action, voteAggregates: aggregates })),
        },
      },
    });
    const row = await svc.get(TX, '0');
    expect(row.vote_aggregates).toEqual(aggregates);
    expect(row.used_epoch_no).toBe(507);
    expect(row.status.expired_epoch).toBe(507);
    expect(networkMetrics).not.toHaveBeenCalled();
  });

  it.each([true, false])(
    'projects passing=%s without leaking provider extensions',
    async (passing) => {
      const aggregate = govAction().voteAggregates![0];
      const extended = {
        ...aggregate,
        privateExtra: 'hidden',
        threshold: { ...aggregate.threshold, privateExtra: 'hidden' },
        passing,
      };
      const row = await service({
        governance: {
          proposals: {
            get: () =>
              Promise.resolve(env(govAction({ voteAggregates: [extended] }))),
          },
        },
      }).get(TX, '0');
      expect(row.vote_aggregates).toEqual([{ ...aggregate, passing }]);
    },
  );

  it('leaves absent aggregates absent rather than synthesizing zero tallies', async () => {
    const row = await service({
      governance: {
        proposals: {
          get: () =>
            Promise.resolve(env(govAction({ voteAggregates: undefined }))),
        },
      },
    }).get(TX, '0');
    expect(row.vote_aggregates).toEqual([]);
  });

  it('sends a previous action index of 0 as "0", which the UI links', async () => {
    const svc = service({
      governance: {
        proposals: {
          get: () =>
            Promise.resolve(
              env(
                govAction({
                  previousAction: {
                    id: actionId(TX2, 0),
                    txHash: TX2,
                    index: 0,
                  },
                }),
              ),
            ),
        },
      },
    });
    const row = await svc.get(TX, '0');
    expect(row.prev_gov_action_index).toBe('0');
    expect(row.prev_gov_action_tx_hash).toBe(TX2);
    const none = await service({
      governance: {
        proposals: { get: () => Promise.resolve(env(govAction())) },
      },
    }).get(TX, '0');
    expect(none.prev_gov_action_index).toBeNull();
  });

  it('is a 404 for an unknown action and a 400 for a malformed hash', async () => {
    const svc = service({
      governance: {
        proposals: {
          get: () =>
            Promise.reject(
              Object.assign(new Error('nope'), { code: 'NOT_FOUND' }),
            ),
        },
      },
    });
    await expect(svc.get(TX, '3')).rejects.toMatchObject({ status: 404 });
    expect(() => svc.get('zz', '0')).toThrow(HttpException);
  });
});

describe('GET /misc/*', () => {
  const stakeStub = jest.fn((q?: { epoch?: number }) =>
    Promise.resolve(env({ ...stake, epoch: q?.epoch ?? 510 })),
  );
  const committeeStub = jest.fn((q?: { epoch?: number }) => {
    void q;
    return Promise.resolve(env(committee));
  });

  beforeEach(() => {
    stakeStub.mockClear();
    committeeStub.mockClear();
  });

  it('network metrics: current figures, DRep total including both predefined targets', async () => {
    const svc = service({
      network: { getStakeDistribution: stakeStub },
      governance: { committee: { getCommittee: committeeStub } },
    });
    await expect(svc.getNetworkMetrics(undefined)).resolves.toEqual({
      epoch_no: 510,
      total_stake_controlled_by_active_dreps: '9007199254741023',
      total_stake_controlled_by_stake_pools: '800',
      always_abstain_voting_power: '10',
      spos_abstain_voting_power: '30',
      always_no_confidence_voting_power: '20',
      spos_no_confidence_voting_power: '40',
      // One seat's term ended at 505.
      no_of_committee_members: 1,
      quorum_numerator: 2,
      quorum_denominator: 3,
    });
    expect(stakeStub).toHaveBeenCalledWith(undefined);
  });

  it('network metrics at a past epoch passes the epoch to a provider that declares it', async () => {
    const svc = service(
      {
        network: { getStakeDistribution: stakeStub },
        governance: { committee: { getCommittee: committeeStub } },
      },
      { optionalArguments: ['stakeDistribution.epoch', 'committee.epoch'] },
    );
    const body = await svc.getNetworkMetrics(504);
    expect(body.epoch_no).toBe(504);
    expect(body.no_of_committee_members).toBe(2);
    expect(stakeStub).toHaveBeenCalledWith({ epoch: 504 });
    expect(committeeStub).toHaveBeenCalledWith({ epoch: 504 });
  });

  it('network metrics at a past epoch is a 501 when the provider cannot, not current figures', async () => {
    const svc = service({
      network: { getStakeDistribution: stakeStub },
      governance: { committee: { getCommittee: committeeStub } },
    });
    await expect(svc.getNetworkMetrics(504)).rejects.toMatchObject({
      status: 501,
    });
    // The current epoch named explicitly needs no declaration.
    await expect(svc.getNetworkMetrics(510)).resolves.toMatchObject({
      epoch_no: 510,
    });
  });

  it('network metrics refuses a stake figure the provider omits', async () => {
    const svc = service({
      network: {
        getStakeDistribution: () =>
          Promise.resolve(
            env({ ...stake, spoAlwaysAbstainVotingPower: undefined }),
          ),
      },
    });
    await expect(svc.getNetworkMetrics(undefined)).rejects.toMatchObject({
      status: 501,
    });
  });

  it('epoch params answers the flat legacy row and passes a declared epoch through', async () => {
    const getProtocolParams = jest.fn((q?: { epoch?: number }) =>
      Promise.resolve(
        env({
          epoch: q?.epoch ?? 510,
          protocolVersion: { major: 10, minor: 0 },
        } as ProtocolParams),
      ),
    );
    const svc = service(
      { network: { getProtocolParams } },
      { optionalArguments: ['protocolParams.epoch'] },
    );
    const row = await svc.getEpochParams(500);
    expect(row).toMatchObject({ epoch_no: 500, protocol_major: 10 });
    expect(getProtocolParams).toHaveBeenCalledWith({ epoch: 500 });
  });
});

describe('GET /governance-actions/proposal/:hash', () => {
  const realFetch = global.fetch;
  afterEach(() => {
    global.fetch = realFetch;
  });

  it('asks the pdf API by submission tx hash and answers its first item', async () => {
    const fetchMock = jest.fn(() =>
      Promise.resolve(
        new Response(
          JSON.stringify({ data: [{ id: 7, attributes: {} }], meta: {} }),
          {
            status: 200,
          },
        ),
      ),
    );
    global.fetch = fetchMock;
    const svc = service({}, { pdfApiUrl: 'http://pdf:1337/api' });
    await expect(svc.getProposal(TX.toUpperCase())).resolves.toEqual({
      data: { id: 7, attributes: {} },
    });
    const url = new URL((fetchMock.mock.calls[0] as unknown as [string])[0]);
    expect(url.origin + url.pathname).toBe('http://pdf:1337/api/proposals');
    expect(url.searchParams.get('filters[prop_submission_tx_hash][$eq]')).toBe(
      TX,
    );
  });

  it('answers { data: null } when no proposal was discussed', async () => {
    global.fetch = () =>
      Promise.resolve(new Response('{"data":[]}', { status: 200 }));
    await expect(
      service({}, { pdfApiUrl: 'http://pdf:1337/api' }).getProposal(TX),
    ).resolves.toEqual({ data: null });
  });

  it('refuses a non-hash before any request, and is a 503 when unconfigured', () => {
    global.fetch = jest.fn();
    const svc = service({}, { pdfApiUrl: 'http://pdf:1337/api' });
    expect(() => svc.getProposal('../users/me')).toThrow(HttpException);
    expect(global.fetch).not.toHaveBeenCalled();
    expect(() => service({}).getProposal(TX)).toThrow(
      expect.objectContaining({ status: 503 }),
    );
  });
});

describe('POST /misc/verify-signature', () => {
  // {1: -8, "address": h'00'}
  const PROTECTED_HEADER = Buffer.concat([
    Buffer.from([0xa2, 0x01, 0x27, 0x67]),
    Buffer.from('address'),
    Buffer.from([0x41, 0x00]),
  ]);
  /** [protected, {}, payload, signature], signed over the Sig_structure. */
  const coseSign1 = (
    protectedHeader: Buffer,
    payload: Buffer,
    key: KeyObject,
  ): Buffer => {
    const toSign = encodeCborArray([
      'Signature1',
      protectedHeader,
      Buffer.alloc(0),
      payload,
    ]);
    return Buffer.concat([
      Buffer.from([0x84, 0x40 + protectedHeader.length]),
      protectedHeader,
      Buffer.from([0xa0, 0x58, 0x20]),
      payload,
      Buffer.from([0x58, 0x40]),
      sign(null, toSign, key),
    ]);
  };
  /** {1: 1 (OKP), 3: -8 (EdDSA), -1: crv, -2: x} */
  const coseKey = (x: Buffer, crv = 6): Buffer =>
    Buffer.concat([
      Buffer.from([0xa4, 0x01, 0x01, 0x03, 0x27, 0x20, crv, 0x21, 0x58, 0x20]),
      x,
    ]);
  const document = {
    '@context': {
      '@language': 'en-us',
      CIP100:
        'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#',
      body: { '@id': 'CIP100:body', '@context': { comment: 'CIP100:comment' } },
    },
    body: { comment: 'An governanceAction' },
  };
  const { publicKey, privateKey } = generateKeyPairSync('ed25519');
  const rawPublicKey = Buffer.from(
    publicKey.export({ format: 'jwk' }).x as string,
    'base64url',
  );

  it('verifies an ed25519 witness over the canonical body hash, and rejects a tampered body', async () => {
    const hash = await hashedBody(document);
    const signature = sign(null, hash, privateKey).toString('hex');
    const author = {
      name: 'a',
      witness: {
        witnessAlgorithm: 'ed25519',
        publicKey: rawPublicKey.toString('hex'),
        signature,
      },
    };
    await expect(verifyAuthorWitness({ author }, document)).resolves.toEqual({
      isValid: true,
      message: 'Signature is valid',
    });
    const tampered = {
      ...document,
      body: { comment: 'Another governanceAction' },
    };
    await expect(
      verifyAuthorWitness({ author }, tampered),
    ).resolves.toMatchObject({
      isValid: false,
    });
  });

  it('verifies a CIP-0008 COSE_Sign1 witness', async () => {
    const hash = Buffer.from(await hashedBody(document));
    const sign1 = coseSign1(PROTECTED_HEADER, hash, privateKey);
    const author = {
      witness: {
        witnessAlgorithm: 'CIP-0008',
        publicKey: rawPublicKey.toString('hex'),
        signature: sign1.toString('hex'),
      },
    };
    await expect(
      verifyAuthorWitness({ author }, document),
    ).resolves.toMatchObject({
      isValid: true,
    });
  });

  it('enforces the COSE_Key and protected-header checks the governanceActions service made', async () => {
    const hash = Buffer.from(await hashedBody(document));
    const witness = (publicKey: Buffer, signature: Buffer) => ({
      author: {
        witness: {
          witnessAlgorithm: 'CIP-0008',
          publicKey: publicKey.toString('hex'),
          signature: signature.toString('hex'),
        },
      },
    });
    const good = coseSign1(PROTECTED_HEADER, hash, privateKey);
    await expect(
      verifyAuthorWitness(witness(coseKey(rawPublicKey), good), document),
    ).resolves.toMatchObject({ isValid: true });
    await expect(
      verifyAuthorWitness(witness(coseKey(rawPublicKey, 4), good), document),
    ).resolves.toEqual({
      isValid: false,
      error: 'COSE_Key map label "-1" (crv) is not "6" (Ed25519)',
    });
    const noAddress = coseSign1(
      Buffer.from([0xa1, 0x01, 0x27]),
      hash,
      privateKey,
    );
    await expect(
      verifyAuthorWitness(witness(rawPublicKey, noAddress), document),
    ).resolves.toEqual({
      isValid: false,
      error: 'Protected header map label "address" is missing',
    });
    const wrongAlg = coseSign1(
      Buffer.concat([
        Buffer.from([0xa2, 0x01, 0x26, 0x67]),
        Buffer.from('address'),
        Buffer.from([0x41, 0x00]),
      ]),
      hash,
      privateKey,
    );
    await expect(
      verifyAuthorWitness(witness(rawPublicKey, wrongAlg), document),
    ).resolves.toEqual({
      isValid: false,
      error: 'Protected header map label "1" (alg) is not "-8" (EdDSA)',
    });
  });

  it('resolves a remote JSON-LD context through the loader to the same hash as the inline one', async () => {
    const url = 'https://example.com/cip100.jsonld';
    const remote = { '@context': url, body: document.body };
    const load = jest.fn(() =>
      Promise.resolve({ '@context': document['@context'] }),
    );
    await expect(hashedBody(remote, load)).resolves.toEqual(
      await hashedBody(document),
    );
    expect(load).toHaveBeenCalledWith(url);
    await expect(
      hashedBody(remote, () => Promise.reject(new Error('URL_BLOCKED'))),
    ).rejects.toThrow(`remote JSON-LD context could not be loaded: ${url}`);
  });

  it('verifies a document with a remote context, fetching both through the guarded fetch', async () => {
    const contextUrl = 'https://example.com/cip100.jsonld';
    const remote = { '@context': contextUrl, body: document.body };
    const signature = sign(null, await hashedBody(document), privateKey);
    const fetchDocumentText = jest.fn((url: string) =>
      Promise.resolve(
        JSON.stringify(
          url === contextUrl ? { '@context': document['@context'] } : remote,
        ),
      ),
    );
    const svc = service({}, { metadata: { fetchDocumentText } });
    await expect(
      svc.verifySignature({
        metadataUrl: 'https://example.com/doc.jsonld',
        author: {
          witness: {
            witnessAlgorithm: 'ed25519',
            publicKey: rawPublicKey.toString('hex'),
            signature: signature.toString('hex'),
          },
        },
      }),
    ).resolves.toEqual({ isValid: true, message: 'Signature is valid' });
    expect(fetchDocumentText).toHaveBeenCalledWith(contextUrl);
  });

  it('never fetches a remote JSON-LD context without a loader', async () => {
    const remote = {
      '@context': 'https://example.com/context.jsonld',
      body: { comment: 'x' },
    };
    const author = {
      witness: {
        witnessAlgorithm: 'ed25519',
        publicKey: '00'.repeat(32),
        signature: '00'.repeat(64),
      },
    };
    const realFetch = global.fetch;
    const fetchMock = jest.fn();
    global.fetch = fetchMock;
    try {
      await expect(
        verifyAuthorWitness({ author }, remote),
      ).resolves.toMatchObject({
        isValid: false,
      });
      expect(fetchMock).not.toHaveBeenCalled();
    } finally {
      global.fetch = realFetch;
    }
  });

  it('fetches the document through the guarded metadata fetch and reports failures as results', async () => {
    const fetchDocumentText = jest.fn(() =>
      Promise.reject(new Error('URL_BLOCKED')),
    );
    const svc = service({}, { metadata: { fetchDocumentText } });
    await expect(
      svc.verifySignature({ metadataUrl: 'http://127.0.0.1/x', author: {} }),
    ).resolves.toEqual({ isValid: false, error: 'Failed to fetch metadata' });
    expect(fetchDocumentText).toHaveBeenCalledWith('http://127.0.0.1/x');
  });
});
