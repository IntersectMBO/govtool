/**
 * Tests for the composed capability document.
 *
 * `src/system/capabilities.ts` is hand-maintained prose about what the code
 * does, so every claim in it needs a test that fails when the code stops doing
 * it. Each `describe` below is one claim, and its name says which:
 *
 *   - a RAISE is tested by driving the service and watching it supply the
 *     capability the provider refused;
 *   - a LOWER is tested by driving the service and watching it destroy one;
 *   - the "no route for it" list is tested by grepping `src/` for
 *     `this.chain.…` call sites, because a new call is exactly the change
 *     that would make that block silently wrong.
 */
import { readFileSync, readdirSync, statSync } from 'node:fs';
import { join } from 'node:path';

import type {
  ChainDataApiV1,
  DRep,
  DRepVoteListQuery,
  Envelope,
  GovAction,
  PageRequest,
  PagedEnvelope,
  ProviderCapabilities,
} from '@govtool/data-providers/chain-data';
import { ChainDataError } from '@govtool/data-providers/chain-data';

import { AdaHolderService } from '../src/ada-holder/ada-holder.service';
import { CacheService } from '../src/cache/cache.service';
import { ConfigService } from '../src/config/config.service';
import { DRepService } from '../src/drep/drep.service';
import { ProposalService } from '../src/proposal/proposal.service';
import { drepIdToCip105 } from '../src/common/legacy-ids';
import {
  allowedOptions,
  BACKEND_PROVIDER_ID,
  backendFeatures,
  capabilityDigest,
  isAvailable,
} from '../src/system/capabilities';
import type { FeatureId } from '../src/system/capabilities';
import { SystemService } from '../src/system/system.service';
import { actionId, drepId } from './ids';

const HASH = 'a'.repeat(56);
const TX = 'd'.repeat(64);
const DREP_ID = drepId('a');
const ACTION_ID = actionId(TX, 0);
/** The legacy reward address the frontend sends: header e0 + HASH. */
const STAKE_KEY = `e0${HASH}`;

const META = { provider: 'stub', network: 'mainnet' } as const;

function env<T>(data: T): Envelope<T> {
  return { data, meta: { ...META } };
}

function page<T>(elements: T[]): PagedEnvelope<T> {
  return env({ elements, total: elements.length });
}

/**
 * A cache with a zero TTL, so each test sees the stub it set up rather than a
 * neighbour's.
 *
 * Both TTL methods are overridden, but the base class also reads
 * `cacheMaxEntries` on every store, so `super` gets a config that answers it.
 * A real `ConfigService` loads `.env` and `config.json` in its constructor,
 * and a unit test must not depend on either.
 */
class ZeroTtlCache extends CacheService {
  constructor() {
    super({
      get: () => ({ cacheMaxEntries: 1_000 }),
    } as unknown as ConfigService);
  }

  defaultTtlSeconds(): number {
    return 0;
  }

  drepListTtlSeconds(): number {
    return 0;
  }
}

function passthroughCache(): CacheService {
  return new ZeroTtlCache();
}

type StubApi = {
  network?: Partial<ChainDataApiV1['network']>;
  accounts?: Partial<ChainDataApiV1['accounts']>;
  system?: Partial<ChainDataApiV1['system']>;
  governance?: {
    dreps?: Partial<ChainDataApiV1['governance']['dreps']>;
    proposals?: Partial<ChainDataApiV1['governance']['proposals']>;
  };
};

function chain(overrides: StubApi): ChainDataApiV1 {
  return overrides as ChainDataApiV1;
}

/**
 * A provider that honours everything it is allowed to. The interesting cases
 * below narrow it; a declaration cannot say a feature is unavailable, only
 * which option values it accepts.
 */
function providerCapabilities(
  overrides: Partial<ProviderCapabilities> = {},
): ProviderCapabilities {
  return {
    sorts: {
      dreps: ['votingPower', 'registrationDate', 'activity', 'random'],
      proposals: ['newest', 'oldest'],
    },
    filters: { dreps: ['status', 'kind'], proposals: ['type', 'status'] },
    search: ['exactId'],
    voteAggregate: ['stake', 'count'],
    optionalArguments: [],
    ...overrides,
  };
}

function drep(overrides: Partial<DRep> = {}): DRep {
  return {
    role: 'drep',
    id: DREP_ID,
    isScriptBased: false,
    kind: 'drep',
    anchor: null,
    registration: {
      latest: {
        txRef: { txHash: 'c'.repeat(64) },
        at: { epoch: 500, time: '2026-01-01T00:00:00.000Z' },
        deposit: '500000000',
      },
      latestUpdate: null,
    },
    status: 'active',
    votingPower: { amount: '100', basis: 'active' },
    activity: { voted: 1, votable: 1 },
    ...overrides,
  };
}

function govAction(overrides: Partial<GovAction> = {}): GovAction {
  return {
    id: ACTION_ID,
    txHash: TX,
    index: 0,
    type: 'InfoAction',
    body: { type: 'InfoAction' },
    lifecycle: {
      status: 'live',
      submitted: { epoch: 500, time: '2026-01-05T00:00:00.000Z' },
      submittedTx: { txHash: TX, index: 0 },
      expires: { epoch: 510, time: '2026-03-01T00:00:00.000Z' },
      ratifiedAt: null,
      enactedAt: null,
      droppedAt: null,
      expiredAt: null,
    },
    anchor: null,
    deposit: null,
    depositReturnAddress: null,
    previousAction: null,
    voteAggregates: [],
    ...overrides,
  };
}

function drepService(dreps: DRep[]): DRepService {
  const cache = passthroughCache();
  const api = chain({
    governance: { dreps: { list: () => Promise.resolve(page(dreps)) } },
    system: { getCapabilities: () => Promise.resolve(env(PROVIDER)) },
  });
  return new DRepService(
    api,
    new ProposalService(api, cache, null),
    cache,
    null,
  );
}

function proposalService(actions: GovAction[]): ProposalService {
  return new ProposalService(
    chain({
      governance: {
        proposals: {
          list: () => Promise.resolve(page(actions)),
          get: (id) => {
            // The contract takes the CIP-129 id; the backend translates the
            // legacy `txHash#index` before asking.
            const found = actions.find((action) => action.id === id);
            return found === undefined
              ? Promise.reject(new ChainDataError('NOT_FOUND', id))
              : Promise.resolve(env(found));
          },
        },
      },
    }),
    passthroughCache(),
    null,
  );
}

/* ------------------------------------------------------------------------- */
/* Source-grepping, for the "no route for it" claims                          */
/* ------------------------------------------------------------------------- */

function sourceFiles(directory: string): string[] {
  return readdirSync(directory).flatMap((entry) => {
    const path = join(directory, entry);
    if (statSync(path).isDirectory()) {
      return sourceFiles(path);
    }
    return path.endsWith('.ts') ? [path] : [];
  });
}

describe('RAISE: sortDReps handles all five DRepSort keys', () => {
  const dreps = [
    drep({
      id: drepId('a'),
      status: 'retired',
      votingPower: { amount: '10', basis: 'active' },
      activity: { voted: 3, votable: 3 },
      registration: {
        latest: {
          txRef: { txHash: TX },
          at: { epoch: 500, time: '2026-01-01T00:00:00.000Z' },
        },
        latestUpdate: null,
      },
    }),
    drep({
      id: drepId('b'),
      status: 'active',
      votingPower: { amount: '30', basis: 'active' },
      activity: { voted: 1, votable: 3 },
      registration: {
        latest: {
          txRef: { txHash: TX },
          at: { epoch: 502, time: '2026-03-01T00:00:00.000Z' },
        },
        latestUpdate: null,
      },
    }),
    drep({
      id: drepId('c'),
      status: 'inactive',
      votingPower: { amount: '20', basis: 'active' },
      activity: { voted: 2, votable: 3 },
      registration: {
        latest: {
          txRef: { txHash: TX },
          at: { epoch: 501, time: '2026-02-01T00:00:00.000Z' },
        },
        latestUpdate: null,
      },
    }),
  ];

  async function order(sort: Parameters<DRepService['list']>[0]['sort']) {
    const body = await drepService(dreps).list({
      status: [],
      page: 0,
      pageSize: 10,
      sort,
    });
    return body.elements.map((element) => element.drepId.slice(-1));
  }

  it('orders by voting power, descending', async () => {
    await expect(order('VotingPower')).resolves.toEqual(['b', 'c', 'a']);
  });

  it('orders by activity, descending', async () => {
    await expect(order('Activity')).resolves.toEqual(['a', 'c', 'b']);
  });

  it('orders by registration date, newest first', async () => {
    await expect(order('RegistrationDate')).resolves.toEqual(['b', 'c', 'a']);
  });

  it('orders by status: active, inactive, retired', async () => {
    await expect(order('Status')).resolves.toEqual(['b', 'c', 'a']);
  });

  it('orders randomly but stably for one seed', async () => {
    const first = await drepService(dreps).list({
      status: [],
      page: 0,
      pageSize: 10,
      sort: 'Random',
      seed: 'abc',
    });
    const again = await drepService(dreps).list({
      status: [],
      page: 0,
      pageSize: 10,
      sort: 'Random',
      seed: 'abc',
    });
    expect(again.elements.map((e) => e.drepId)).toEqual(
      first.elements.map((e) => e.drepId),
    );
    expect(first.elements).toHaveLength(3);
  });
});

describe('LOWER: sortProposals silently ignores the two keys it has no case for', () => {
  const actions = [govAction({ index: 0 }), govAction({ index: 1 })];

  it('returns the list untouched for an unknown sort', async () => {
    const service = proposalService(actions);
    const unsorted = await service.list({ type: [], page: 0, pageSize: 10 });
    // `GovernanceActionSortMode` has no member for either, and the switch
    // falls through `default: return copied` — no error, no ordering.
    const asked = service.sortProposals(
      unsorted.elements,
      'HighestParticipation' as never,
    );
    expect(asked.map((e) => e.index)).toEqual([0, 1]);
  });
});

describe('LOWER: drepId is accepted on the proposal routes and dropped', () => {
  it('always answers vote: null, however the caller identifies itself', async () => {
    const service = proposalService([govAction()]);
    const body = await service.get(`${TX}#0`, HASH);
    expect(body.vote).toBeNull();
  });
});

describe('RAISE: the status filter and the pager run over the whole snapshot', () => {
  const dreps = [
    drep({ id: drepId('a'), status: 'active' }),
    drep({ id: drepId('b'), status: 'inactive' }),
    drep({ id: drepId('c'), status: 'retired' }),
  ];

  it('filters by any combination of the three statuses', async () => {
    const body = await drepService(dreps).list({
      status: ['Active', 'Retired'],
      page: 0,
      pageSize: 10,
    });
    expect(body.elements.map((e) => e.status)).toEqual(['Active', 'Retired']);
    expect(body.total).toBe(2);
  });

  it('reports an exact total over the filtered set, not the page', async () => {
    const body = await drepService(dreps).list({
      status: [],
      page: 1,
      pageSize: 2,
    });
    expect(body.total).toBe(3);
    expect(body.elements).toHaveLength(1);
  });
});

describe('RAISE: the proposal list is filtered and sorted in memory', () => {
  const actions = [
    govAction({ index: 0, type: 'InfoAction' }),
    govAction({
      id: actionId(TX, 1),
      index: 1,
      type: 'ParameterChange',
      body: { type: 'ParameterChange', changes: { drepActivity: 20 } },
      lifecycle: {
        status: 'live',
        submitted: { epoch: 501, time: '2026-02-05T00:00:00.000Z' },
        submittedTx: { txHash: TX, index: 1 },
        expires: { epoch: 505, time: '2026-02-01T00:00:00.000Z' },
        ratifiedAt: null,
        enactedAt: null,
        droppedAt: null,
        expiredAt: null,
      },
      voteAggregates: [
        {
          role: 'drep',
          representation: 'stake',
          yes: '9',
          no: '0',
          abstain: '0',
          notVoted: '0',
          totalEligible: '9',
          threshold: { numerator: 67, denominator: 100 },
        },
      ],
    }),
  ];

  it('filters by action type over the whole snapshot', async () => {
    const body = await proposalService(actions).list({
      type: ['ParameterChange'],
      page: 0,
      pageSize: 10,
    });
    expect(body.total).toBe(1);
    expect(body.elements[0].index).toBe(1);
  });

  /**
   * The provider declares `exactId` search only, and the backend still matches
   * a substring — of the action id, which is what it has. The four CIP-108
   * strings the legacy search also matched are metadata, and chain data no
   * longer resolves any, so free text over a title is gone until a metadata
   * service is wired in.
   */
  it('searches over the whole snapshot, not by exact id alone', async () => {
    const body = await proposalService(actions).list({
      type: [],
      page: 0,
      pageSize: 10,
      search: actionId(TX, 1),
    });
    expect(body.total).toBe(1);
    expect(body.elements[0].index).toBe(1);
  });

  it('sorts newest-first, soonest-to-expire and most-yes-votes', async () => {
    const service = proposalService(actions);
    const newest = await service.list({
      type: [],
      page: 0,
      pageSize: 10,
      sort: 'NewestCreated',
    });
    expect(newest.elements.map((e) => e.index)).toEqual([1, 0]);

    const expiring = await service.list({
      type: [],
      page: 0,
      pageSize: 10,
      sort: 'SoonestToExpire',
    });
    expect(expiring.elements.map((e) => e.index)).toEqual([1, 0]);

    const yes = await service.list({
      type: [],
      page: 0,
      pageSize: 10,
      sort: 'MostYesVotes',
    });
    expect(yes.elements.map((e) => e.index)).toEqual([1, 0]);
  });
});

describe('LOWER: laundering an absence into a number', () => {
  it('turns a provider failure into 0 voting power for the connected wallet', async () => {
    const service = new AdaHolderService(
      chain({
        accounts: {
          getVotingPower: () =>
            Promise.reject(new ChainDataError('PROVIDER_UNAVAILABLE', 'down')),
        },
      }),
      passthroughCache(),
    );
    // Not a 503: the number the browser renders as fact.
    await expect(service.getVotingPower(STAKE_KEY)).resolves.toBe(0);
  });

  it('turns "no rows" into the same 0, so the two are indistinguishable', async () => {
    const service = new AdaHolderService(
      chain({ accounts: { getVotingPower: () => Promise.resolve(env(null)) } }),
      passthroughCache(),
    );
    await expect(service.getVotingPower(STAKE_KEY)).resolves.toBe(0);
  });

  it('turns an unknown DRep voting power into 0 on the batch route', async () => {
    const cache = passthroughCache();
    const api = chain({
      governance: {
        dreps: { get: () => Promise.resolve(env(drep({ votingPower: null }))) },
      },
    });
    const service = new DRepService(
      api,
      new ProposalService(api, cache, null),
      cache,
      null,
    );
    await expect(service.getVotingPowerList([DREP_ID])).resolves.toEqual([
      {
        view: drepIdToCip105(DREP_ID),
        hashRaw: HASH,
        votingPower: 0,
        givenName: null,
      },
    ]);
  });

  it('turns an unknown deposit into 0 on the directory row', async () => {
    const body = await drepService([
      drep({
        registration: {
          latest: { txRef: { txHash: TX }, at: { epoch: 500 } },
          latestUpdate: null,
        },
      }),
    ]).list({ status: [], page: 0, pageSize: 10 });
    expect(body.elements[0].deposit).toBe(0);
  });
});

describe('RAISE: a DRep vote history is read whole, not one provider page', () => {
  it('pages the listing to the end and asks for voted rows only', async () => {
    const calls: (PageRequest & DRepVoteListQuery)[] = [];
    const cache = passthroughCache();
    const api = chain({
      governance: {
        dreps: {
          listVotes: (_id, q) => {
            calls.push(q);
            return Promise.resolve(env({ elements: [], total: 0 }));
          },
        },
        proposals: { list: () => Promise.resolve(page([govAction()])) },
      },
    });
    const service = new DRepService(
      api,
      new ProposalService(api, cache, null),
      cache,
      null,
    );

    await service.getVotes(DREP_ID);

    // A paged read, from page 1, restricted to the rows the legacy endpoint
    // returns — the listing also carries not-voted rows.
    expect(calls).toHaveLength(1);
    expect(calls[0].page).toBe(1);
    expect(calls[0].voted).toBe(true);
  });

  it('refuses rather than answering an empty list when the provider has no listing', async () => {
    const cache = passthroughCache();
    const api = chain({ governance: { dreps: {} } });
    const service = new DRepService(
      api,
      new ProposalService(api, cache, null),
      cache,
      null,
    );
    await expect(service.getVotes(DREP_ID)).rejects.toMatchObject({
      status: 501,
    });
  });
});

/* ------------------------------------------------------------------------- */
/* The composed feature set                                                   */
/* ------------------------------------------------------------------------- */

const PROVIDER = providerCapabilities();
const COMPOSED = backendFeatures(PROVIDER, 'mainnet');

describe('the composed feature set', () => {
  it('claims this backend, not the provider', () => {
    expect(COMPOSED.provider).toBe(BACKEND_PROVIDER_ID);
    expect(COMPOSED.network).toBe('mainnet');
  });

  it('RAISES the controls it applies to its own snapshot', () => {
    // The point is that the backend removes the restriction entirely, so a
    // provider that refuses every key still gets a full menu.
    const raised = backendFeatures(
      providerCapabilities({
        sorts: { dreps: [], proposals: ['newest'] },
        filters: { dreps: [], proposals: [] },
        search: ['exactId'],
      }),
      'mainnet',
    );
    const universe = ['votingPower', 'activity', 'status'];
    expect(allowedOptions(raised, 'drepDirectory.sort', universe)).toEqual(
      universe,
    );
    expect(allowedOptions(raised, 'govActionList.sort', ['newest'])).toEqual([
      'newest',
    ]);
    expect(
      allowedOptions(raised, 'drepDirectory.search', ['exactId', 'freeText']),
    ).toEqual(['exactId', 'freeText']);
    expect(allowedOptions(raised, 'govActionList.status', ['live'])).toEqual([
      'live',
    ]);
  });

  it('LOWERS a feature whose route it does not expose', () => {
    // The provider can serve these; this backend has no route for them.
    expect(isAvailable(COMPOSED, 'network.treasury')).toBe(false);
    expect(isAvailable(COMPOSED, 'committee.browse')).toBe(false);
    expect(isAvailable(COMPOSED, 'spo.directory')).toBe(false);
    expect(isAvailable(COMPOSED, 'govAction.voterList')).toBe(false);
  });

  it('keeps the features it does expose available', () => {
    for (const feature of [
      'drep.directory',
      'govAction.list',
      'account.currentDelegation',
      'dashboard.metrics',
    ] as FeatureId[]) {
      expect(isAvailable(COMPOSED, feature)).toBe(true);
    }
  });

  it('fails open on a feature set that could not be fetched', () => {
    // An unreachable /system/features must never hide a working screen.
    expect(isAvailable(undefined, 'committee.browse')).toBe(true);
    expect(
      allowedOptions(undefined, 'drepDirectory.sort', ['votingPower']),
    ).toEqual(['votingPower']);
  });

  it('carries the losses it introduces itself', () => {
    const votingPower = COMPOSED.caveats.filter(
      (c) => c.feature === 'account.votingPower',
    );
    expect(votingPower).toHaveLength(1);
    expect(votingPower[0].note).toMatch(/returns 0/);

    // The metrics route now assembles its counters, and says which of them no
    // resource owns any more.
    const metrics = COMPOSED.caveats.filter(
      (c) => c.feature === 'dashboard.metrics',
    );
    expect(metrics).toHaveLength(1);
    expect(metrics[0].kind).toBe('derived');

    // Every metadata-derived field on a directory row is null.
    expect(
      COMPOSED.caveats.some(
        (c) => c.feature === 'drep.directory' && /anchor/.test(c.note),
      ),
    ).toBe(true);
  });

  it('never qualifies a feature it also declares unavailable', () => {
    for (const caveat of COMPOSED.caveats) {
      expect(COMPOSED.unavailable[caveat.feature]).toBeUndefined();
    }
  });

  it('digests to a stable value', () => {
    expect(capabilityDigest(COMPOSED)).toBe(capabilityDigest(COMPOSED));
    expect(capabilityDigest(COMPOSED)).toMatch(/^sha256:[0-9a-f]{16}$/);
  });
});

describe('the features this backend has no route for', () => {
  // A new `this.chain.*` call is what would make the NO_ROUTE block wrong, so
  // the list of called routes is grepped rather than read.
  const called = new Set<string>();
  for (const file of sourceFiles(join(__dirname, '..', 'src'))) {
    for (const match of readFileSync(file, 'utf8').matchAll(
      /this\.chain\.([A-Za-z0-9_]+(?:\.[A-Za-z0-9_]+)*)/g,
    )) {
      called.add(match[1]);
    }
  }

  it.each([
    ['network.treasury', 'network.getTreasury'],
    ['spo.directory', 'governance.pools.list'],
    ['govAction.voterList', 'governance.proposals.listVotes'],
    ['govAction.activityTimeline', 'governance.proposals.listActivity'],
    ['drep.delegatorList', 'governance.dreps.listDelegators'],
    ['drep.registrationHistory', 'governance.dreps.listUpdateHistory'],
    ['account.delegationHistory', 'accounts.listDelegationHistory'],
  ])('%s is declared unavailable and %s is never called', (feature, route) => {
    expect(isAvailable(COMPOSED, feature as FeatureId)).toBe(false);
    expect(called.has(route)).toBe(false);
  });

  /**
   * The one exception, and the reason the two halves are tested separately:
   * `GovernanceMetrics` is gone, so the committee's size and quorum come from
   * the committee itself. The call exists; a route to browse the committee
   * still does not.
   */
  it('reads the committee for the metrics counters without exposing it', () => {
    expect(called.has('governance.committee.getCommittee')).toBe(true);
    expect(isAvailable(COMPOSED, 'committee.browse')).toBe(false);
    expect(called.has('governance.committee.getMember')).toBe(false);
    expect(called.has('governance.committee.getConstitution')).toBe(false);
  });

  it('does call the routes behind the features it keeps', () => {
    for (const route of [
      'governance.dreps.list',
      'governance.proposals.list',
      'accounts.getDelegation',
    ]) {
      expect(called.has(route)).toBe(true);
    }
  });
});

describe('the /system endpoints', () => {
  it('serves the provider declaration raw and the feature set composed', async () => {
    const api = chain({
      system: { getCapabilities: () => Promise.resolve(env(PROVIDER)) },
    });
    const system = new SystemService(api, passthroughCache());

    const raw = await system.getCapabilities();
    expect(raw.search).toEqual(['exactId']);
    expect(raw.sorts.proposals).toContain('newest');

    const features = await system.getFeatures();
    expect(features.provider).toBe(BACKEND_PROVIDER_ID);
    // From the envelope the declaration arrived in.
    expect(features.network).toBe('mainnet');
    expect(isAvailable(features, 'committee.browse')).toBe(false);
  });
});
