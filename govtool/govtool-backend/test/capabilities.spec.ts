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
 *   - the route list is tested by grepping `src/` rather than by reading it,
 *     because a new `this.chain.…` call is exactly the change that would make
 *     the "not exposed" block silently wrong.
 */
import { readFileSync, readdirSync, statSync } from 'node:fs';
import { join } from 'node:path';

import type {
  AnyDatasetCapability,
  Caveat,
  ChainDataApiV1,
  DRep,
  DatasetId,
  GovAction,
  ProviderCapabilityDocument,
  RouteId,
} from '@govtool/data-providers/chain-data';
import {
  DATASETS,
  DATASET_IDS,
  ChainDataError,
  composeCapabilities,
  declarationProblems,
  readCapability,
  resolveCapabilities,
} from '@govtool/data-providers/chain-data';
import { dbSyncCapabilities } from '@govtool/provider-dbsync';

import { AdaHolderService } from '../src/ada-holder/ada-holder.service';
import { CacheService } from '../src/cache/cache.service';
import { ConfigService } from '../src/config/config.service';
import { DRepService } from '../src/drep/drep.service';
import { ProposalService } from '../src/proposal/proposal.service';
import {
  BACKEND_CHAIN_DATA_ROUTES,
  BACKEND_PROVIDER_ID,
  backendFeatures,
  composeBackendCapabilities,
  notExposedByBackend,
} from '../src/system/capabilities';
import { SystemService } from '../src/system/system.service';

const HASH = 'a'.repeat(56);
const TX = 'd'.repeat(64);

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

function drep(overrides: Partial<DRep> = {}): DRep {
  return {
    role: 'drep',
    id: 'drep1cip129',
    hash: HASH,
    isScriptBased: false,
    cip105Id: 'drep1legacyview',
    kind: 'drep',
    registration: {
      status: 'active',
      registeredAt: { time: '2026-01-01T00:00:00.000Z' },
      registrationTx: { txHash: 'c'.repeat(64) },
      deposit: '500000000',
    },
    metadata: null,
    votingPower: { amount: '100', basis: 'active' },
    activity: { votesCast: 1 },
    ...overrides,
  };
}

function govAction(overrides: Partial<GovAction> = {}): GovAction {
  return {
    id: 'gov_action1abc',
    txHash: TX,
    index: 0,
    providerId: '42',
    type: 'InfoAction',
    rawBody: { data: {} },
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
    previousAction: null,
    metadata: null,
    tallies: [],
    ...overrides,
  };
}

function drepService(dreps: DRep[]): DRepService {
  const cache = passthroughCache();
  const api = chain({
    governance: {
      dreps: {
        list: () =>
          Promise.resolve({
            meta: {},
            data: { nextCursor: null, elements: dreps },
          }),
      },
    },
  });
  return new DRepService(api, new ProposalService(api, cache), cache);
}

function proposalService(actions: GovAction[]): ProposalService {
  return new ProposalService(
    chain({
      governance: {
        proposals: {
          list: () =>
            Promise.resolve({
              meta: {},
              data: { nextCursor: null, elements: actions },
            }),
          get: (id) => {
            const found = actions.find(
              (action) => `${action.txHash}#${action.index}` === id,
            );
            return found === undefined
              ? Promise.reject(new ChainDataError('NOT_FOUND', id))
              : Promise.resolve({ meta: {}, data: found });
          },
        },
      },
    }),
    passthroughCache(),
  );
}

/** The real db-sync declaration, which is what this backend is wired to. */
const DBSYNC: ProviderCapabilityDocument = dbSyncCapabilities('mainnet');
const COMPOSED = composeBackendCapabilities(DBSYNC);

function dataset(doc: ProviderCapabilityDocument, id: DatasetId) {
  return readCapability(resolveCapabilities(doc), id);
}

/* ------------------------------------------------------------------------- */
/* The route list the "not exposed" block is derived from                     */
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

describe('BACKEND_CHAIN_DATA_ROUTES', () => {
  it('names exactly the contract routes src/ calls', () => {
    const called = new Set<string>();
    for (const file of sourceFiles(join(__dirname, '..', 'src'))) {
      const source = readFileSync(file, 'utf8');
      for (const match of source.matchAll(
        /this\.chain\.([A-Za-z0-9_.]+?)\(/g,
      )) {
        // `system.*` is how a consumer reads this document, not a gated route.
        if (!match[1].startsWith('system.')) {
          called.add(match[1]);
        }
      }
    }

    expect([...called].sort()).toEqual([...BACKEND_CHAIN_DATA_ROUTES].sort());
  });

  it('only lists routes the contract actually has', () => {
    const contractRoutes = new Set<string>(
      DATASET_IDS.flatMap((id) => [
        ...(DATASETS[id].routes as readonly string[]),
      ]),
    );
    for (const route of BACKEND_CHAIN_DATA_ROUTES) {
      expect(contractRoutes.has(route)).toBe(true);
    }
  });
});

/* ------------------------------------------------------------------------- */
/* RAISE — the DRep directory                                                 */
/* ------------------------------------------------------------------------- */

describe('RAISE: sortDReps handles all five DRepSort keys', () => {
  const dreps = [
    drep({
      hash: 'a'.repeat(56),
      votingPower: { amount: '10', basis: 'active' },
      activity: { votesCast: 3 },
      registration: {
        status: 'retired',
        registeredAt: { time: '2026-01-01T00:00:00.000Z' },
        deposit: '1',
      },
    }),
    drep({
      hash: 'b'.repeat(56),
      votingPower: { amount: '30', basis: 'active' },
      activity: { votesCast: 1 },
      registration: {
        status: 'active',
        registeredAt: { time: '2026-03-01T00:00:00.000Z' },
        deposit: '1',
      },
    }),
    drep({
      hash: 'c'.repeat(56),
      votingPower: { amount: '20', basis: 'active' },
      activity: { votesCast: 2 },
      registration: {
        status: 'inactive',
        registeredAt: { time: '2026-02-01T00:00:00.000Z' },
        deposit: '1',
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
    return body.elements.map((element) => element.drepId[0]);
  }

  it('orders by voting power, descending', async () => {
    await expect(order('VotingPower')).resolves.toEqual(['b', 'c', 'a']);
  });

  it('orders by activity (votes in the trailing window), descending', async () => {
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

  it('is declared honoured for every key, even when the provider refuses them all', () => {
    // A Koios-shaped base: every DRepSort key rejected, because /drep_list has
    // nothing to order 1,000 arbitrary rows by. The composed document must
    // still offer the sort, since the backend supplies it.
    const sortless: ProviderCapabilityDocument = {
      ...DBSYNC,
      datasets: composeCapabilities(DBSYNC.datasets, {
        'drep.identity.current': {
          sort: {
            votingPower: 'rejected',
            registrationDate: 'rejected',
            activity: 'rejected',
            status: 'rejected',
            random: 'rejected',
          },
        },
      }),
    };

    const composed = composeBackendCapabilities(sortless);
    expect(dataset(composed, 'drep.identity.current').sort).toEqual({
      votingPower: 'honoured',
      registrationDate: 'honoured',
      activity: 'honoured',
      status: 'honoured',
      random: 'honoured',
    });

    const features = backendFeatures(composed);
    expect(features.features['drepDirectory.browse'].available).toBe(true);
    expect(
      features.features['drepDirectory.browse'].options.sort.allowed,
    ).toEqual([
      'votingPower',
      'registrationDate',
      'activity',
      'status',
      'random',
    ]);
  });

  it('declares a sort ignored when the provider never fills the column it orders by', () => {
    // The backend supplies the ordering; the provider supplies the column. On
    // a provider that serves `DRep.activity` only under `expand`, the backend
    // never sees it — no service passes `expand` — so `sort: 'activity'` runs
    // over an array of nulls and produces an unsorted list presented as
    // sorted. That is `ignored`, and a UI must not offer it.
    const expandOnly: ProviderCapabilityDocument = {
      ...DBSYNC,
      entities: {
        ...DBSYNC.entities,
        DRep: {
          ...DBSYNC.entities.DRep,
          fields: {
            ...DBSYNC.entities.DRep.fields,
            activity: { serves: 'onExpand' },
          },
        },
      },
      fieldOverrides: DBSYNC.fieldOverrides.filter(
        (override) =>
          !(override.entity === 'DRep' && override.field === 'activity'),
      ),
    };

    const sort = dataset(
      composeBackendCapabilities(expandOnly),
      'drep.identity.current',
    ).sort;
    expect(sort?.activity).toBe('ignored');
    // The other four are unaffected: one dead column is one lost menu item.
    expect(sort?.votingPower).toBe('honoured');
    expect(sort?.status).toBe('honoured');
  });
});

describe('RAISE: the status filter and the pager run over the whole snapshot', () => {
  const dreps = [
    drep({
      hash: 'a'.repeat(56),
      registration: { status: 'active', deposit: '1' },
    }),
    drep({
      hash: 'b'.repeat(56),
      registration: { status: 'inactive', deposit: '1' },
    }),
    drep({
      hash: 'c'.repeat(56),
      registration: { status: 'retired', deposit: '1' },
    }),
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

  it('declares all three statuses honoured and an exact total', () => {
    const cap = dataset(COMPOSED, 'drep.identity.current');
    expect(cap.filters?.status?.values).toEqual({
      active: 'honoured',
      inactive: 'honoured',
      retired: 'honoured',
    });
    expect(cap.paging).toMatchObject({
      offset: 'honoured',
      total: 'exact',
      defaultLimit: 10,
    });
  });
});

/* ------------------------------------------------------------------------- */
/* RAISE — the governance-action list                                         */
/* ------------------------------------------------------------------------- */

describe('RAISE: the proposal list is filtered, searched and sorted in memory', () => {
  const actions = [
    govAction({
      index: 0,
      type: 'InfoAction',
      metadata: {
        id: 'm0',
        anchor: { url: 'u', dataHash: 'h' },
        standard: 'CIP108',
        status: 'valid',
        body: { title: 'Treasury plans', authors: [] },
      },
    }),
    govAction({
      index: 1,
      type: 'ParameterChange',
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
      tallies: [{ role: 'drep', stake: { yes: '9', no: '0', abstain: '0' } }],
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

  it('searches free text across the metadata body, not just the id', async () => {
    const body = await proposalService(actions).list({
      type: [],
      page: 0,
      pageSize: 10,
      search: 'treasury pl',
    });
    expect(body.total).toBe(1);
    expect(body.elements[0].title).toBe('Treasury plans');
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

  it('declares the three it applies honoured, and free text honoured', () => {
    const cap = dataset(COMPOSED, 'proposal.identity.current');
    expect(cap.sort).toMatchObject({
      newest: 'honoured',
      soonestToExpire: 'honoured',
      mostYesVotes: 'honoured',
    });
    expect(cap.search?.modes.freeText).toBe('honoured');
    expect(cap.filters?.type?.values.UpdateCommittee).toBe('honoured');
  });
});

/* ------------------------------------------------------------------------- */
/* LOWER — sorts and parameters the backend accepts and drops                  */
/* ------------------------------------------------------------------------- */

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

  it('declares oldest and highestParticipation ignored, not rejected', () => {
    const cap = dataset(COMPOSED, 'proposal.identity.current');
    expect(cap.sort?.oldest).toBe('ignored');
    expect(cap.sort?.highestParticipation).toBe('ignored');
  });

  it('keeps them out of the UI option set, since ignored is never offerable', () => {
    const allowed =
      backendFeatures(COMPOSED).features['govActionList.browse'].options.sort
        .allowed;
    expect(allowed).not.toContain('oldest');
    expect(allowed).not.toContain('highestParticipation');
    expect(allowed).toContain('newest');
  });
});

describe('LOWER: drepId is accepted on the proposal routes and dropped', () => {
  it('always answers vote: null, however the caller identifies itself', async () => {
    const service = proposalService([govAction()]);
    const body = await service.get(`${TX}#0`, HASH);
    expect(body.vote).toBeNull();
  });

  it('declares the callerVote join ignored, so the badge is not offered', () => {
    expect(
      dataset(COMPOSED, 'proposal.identity.current').joins?.callerVote,
    ).toBe('ignored');
    expect(
      backendFeatures(COMPOSED).features['govAction.myVoteBadge'].available,
    ).toBe(false);
  });
});

describe('LOWER: no service passes expand, so no expand is requestable', () => {
  it('reads the DRep directory without an expand argument', async () => {
    const calls: unknown[] = [];
    const cache = passthroughCache();
    const api = chain({
      governance: {
        dreps: {
          list: (query) => {
            calls.push(query);
            return Promise.resolve({
              meta: {},
              data: { nextCursor: null, elements: [drep()] },
            });
          },
        },
      },
    });
    const service = new DRepService(
      api,
      new ProposalService(api, cache),
      cache,
    );
    await service.list({ status: [], page: 0, pageSize: 10 });

    expect(calls).toHaveLength(1);
    expect(calls[0]).not.toHaveProperty('expand');
  });

  it('declares every expand member ignored on the datasets it reads', () => {
    expect(dataset(COMPOSED, 'drep.identity.current').expand).toEqual({
      metadata: 'ignored',
      liveVotingPower: 'ignored',
      delegators: 'ignored',
      activity: 'ignored',
    });
    expect(
      Object.values(
        dataset(COMPOSED, 'proposal.identity.current').expand ?? {},
      ),
    ).toEqual(
      ['tallies', 'thresholds', 'metadata', 'myVote', 'protocolParams'].map(
        () => 'ignored',
      ),
    );
  });
});

/* ------------------------------------------------------------------------- */
/* LOWER — absence laundered into a plausible value                            */
/* ------------------------------------------------------------------------- */

describe('LOWER: laundering an absence into a number', () => {
  function misreport(
    entity: 'VotingPower' | 'DRepVotingPowerEntry' | 'Registration',
  ) {
    return COMPOSED.entities[entity].misreported ?? [];
  }

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
    await expect(service.getVotingPower(HASH)).resolves.toBe(0);
  });

  it('turns "no rows" into the same 0, so the two are indistinguishable', async () => {
    const service = new AdaHolderService(
      chain({
        accounts: {
          getVotingPower: () => Promise.resolve({ meta: {}, data: null }),
        },
      }),
      passthroughCache(),
    );
    await expect(service.getVotingPower(HASH)).resolves.toBe(0);
  });

  it('declares VotingPower.amount misreported as 0', () => {
    expect(misreport('VotingPower')).toContainEqual(
      expect.objectContaining({ field: 'amount', sends: '0' }),
    );
  });

  it('turns an unknown DRep voting power into 0 on the batch route', async () => {
    const cache = passthroughCache();
    const api = chain({
      governance: {
        dreps: {
          getVotingPowers: () =>
            Promise.resolve({
              meta: {},
              data: [
                {
                  subject: {
                    kind: 'drep' as const,
                    drep: {
                      role: 'drep' as const,
                      id: 'drep1x',
                      hash: HASH,
                      isScriptBased: false,
                    },
                  },
                  votingPower: null,
                },
              ],
            }),
        },
      },
    });
    const service = new DRepService(
      api,
      new ProposalService(api, cache),
      cache,
    );
    await expect(service.getVotingPowerList([HASH])).resolves.toEqual([
      { view: 'drep1x', hashRaw: HASH, votingPower: 0, givenName: null },
    ]);
  });

  it('declares DRepVotingPowerEntry.votingPower misreported as 0', () => {
    expect(misreport('DRepVotingPowerEntry')).toContainEqual(
      expect.objectContaining({ field: 'votingPower', sends: '0' }),
    );
  });

  it('turns an unknown deposit into 0 on the directory row', async () => {
    const body = await drepService([
      drep({ registration: { status: 'active', deposit: null } }),
    ]).list({ status: [], page: 0, pageSize: 10 });
    expect(body.elements[0].deposit).toBe(0);
  });

  it('declares Registration.deposit misreported as 0', () => {
    expect(misreport('Registration')).toContainEqual(
      expect.objectContaining({ field: 'deposit', sends: '0' }),
    );
  });
});

describe('LOWER: a DRep vote history is one provider page, unpaged', () => {
  it('asks listVotes for no page and follows no cursor', async () => {
    const calls: unknown[] = [];
    const cache = passthroughCache();
    const api = chain({
      governance: {
        dreps: {
          listVotes: (...args: unknown[]) => {
            calls.push(args);
            return Promise.resolve({
              meta: {},
              data: { nextCursor: 'more-after-this', elements: [] },
            });
          },
        },
      },
    });
    const service = new DRepService(
      api,
      new ProposalService(api, cache),
      cache,
    );

    await service.getVotes(HASH);

    // One call, with the id alone — no PageRequest, and the cursor the
    // provider returned is never followed.
    expect(calls).toEqual([[HASH]]);
  });

  it('declares the truncation as a notExhaustive caveat', () => {
    const caveats: readonly Caveat[] =
      dataset(COMPOSED, 'drep.ballot.current').caveats ?? [];
    expect(caveats.map((caveat) => caveat.kind)).toContain('notExhaustive');
    // The provider's own caveats survive the merge: db-sync warns that this
    // route omits votes cast on concluded actions, and that is still true.
    const providerCaveats: readonly Caveat[] =
      dataset(DBSYNC, 'drep.ballot.current').caveats ?? [];
    for (const caveat of providerCaveats) {
      expect(caveats).toContainEqual(caveat);
    }
    expect(providerCaveats.length).toBeGreaterThan(0);
  });
});

/* ------------------------------------------------------------------------- */
/* LOWER — routes this backend does not have                                  */
/* ------------------------------------------------------------------------- */

describe('LOWER: datasets no controller reads are refused', () => {
  const notExposed = notExposedByBackend();

  it('covers every dataset none of whose routes the backend calls', () => {
    const called = new Set<string>(BACKEND_CHAIN_DATA_ROUTES);
    for (const id of DATASET_IDS) {
      const reachable = (DATASETS[id].routes as readonly string[]).some(
        (route) => called.has(route),
      );
      if (!reachable) {
        expect(notExposed[id]).toBeDefined();
      }
    }
  });

  it('refuses a dataset db-sync really does serve', () => {
    // The one live instance today: db-sync projects a vote's CIP-100
    // rationale, and this backend calls `governance.votes.get` nowhere, so a
    // browser told the feature works would call an endpoint that 404s.
    expect(dataset(DBSYNC, 'vote.metadata.current').reachability).toBe(
      'served',
    );
    expect(dataset(COMPOSED, 'vote.metadata.current')).toMatchObject({
      reachability: 'refused',
      unavailable: { kind: 'notImplemented', scope: 'source' },
    });
  });

  it('refuses every one of them on a provider that serves them all', () => {
    // db-sync happens to refuse most of these itself, which would leave the
    // block untested. A provider that serves everything proves the backend's
    // own limit is what the composed document reports.
    const everything: Partial<
      Record<DatasetId, Partial<AnyDatasetCapability>>
    > = {};
    for (const id of DATASET_IDS) {
      if (notExposed[id] !== undefined) {
        everything[id] = {
          reachability: 'served',
          unavailable: undefined,
          pollable: true,
        };
      }
    }
    const generous: ProviderCapabilityDocument = {
      ...DBSYNC,
      datasets: composeCapabilities(DBSYNC.datasets, everything),
    };

    const composed = composeBackendCapabilities(generous);
    for (const id of Object.keys(notExposed) as DatasetId[]) {
      expect(dataset(composed, id)).toMatchObject({
        reachability: 'refused',
        unavailable: { kind: 'notImplemented', scope: 'source' },
      });
    }
    // Spot-check the two the frontend would most obviously be misled by.
    const features = backendFeatures(composed).features;
    expect(features['govAction.voterList'].available).toBe(false);
    expect(features['committee.browse'].available).toBe(false);
  });

  it('keeps the provider reason when the provider already refuses', () => {
    // `drep.delegation.events` is a universal gap; "no source records the
    // transitions" outranks "this backend has no route", and the derived
    // feature must still come out permanentlyAbsent.
    expect(
      dataset(COMPOSED, 'drep.delegation.events').unavailable?.reason,
    ).toEqual(dataset(DBSYNC, 'drep.delegation.events').unavailable?.reason);
    expect(
      backendFeatures(COMPOSED).features['drep.delegationTimeline']
        .permanentlyAbsent,
    ).toBe(true);
  });

  it('leaves the datasets the backend does read alone', () => {
    for (const id of [
      'drep.identity.current',
      'proposal.identity.current',
      'network.identity.current',
      'account.delegation.current',
      'transaction.identity.current',
    ] satisfies DatasetId[]) {
      expect(dataset(COMPOSED, id).reachability).toBe('served');
    }
  });
});

/* ------------------------------------------------------------------------- */
/* The document as a whole                                                    */
/* ------------------------------------------------------------------------- */

describe('the composed document', () => {
  it('has no declaration problems', () => {
    expect(declarationProblems(COMPOSED)).toEqual([]);
  });

  it('says who it was composed from, rather than impersonating the provider', () => {
    expect(COMPOSED.provider).toBe(BACKEND_PROVIDER_ID);
    expect(COMPOSED.composedFrom).toEqual([
      { provider: 'dbsync', network: 'mainnet' },
    ]);
    expect(COMPOSED.network).toBe('mainnet');
  });

  it('is deterministic, so a consumer can compare digests', () => {
    expect(backendFeatures(COMPOSED).sourceDigest).toBe(
      backendFeatures(composeBackendCapabilities(DBSYNC)).sourceDigest,
    );
  });

  it('lets a provider deployment fault still win over a compensation', () => {
    // A fault is applied by `resolveCapabilities` after composition: the
    // backend can page and sort rows it could not read in the first place.
    const faulty: ProviderCapabilityDocument = {
      ...DBSYNC,
      overrides: [
        {
          dataset: 'drep.identity.current',
          reachability: 'refused',
          unavailable: {
            kind: 'deploymentFault',
            scope: 'deployment',
            symptom: 'statement timeout',
            reason: 'list-dreps.sql times out on this instance.',
          },
        },
      ],
    };
    const composed = composeBackendCapabilities(faulty);
    expect(dataset(composed, 'drep.identity.current').reachability).toBe(
      'refused',
    );
    const browse = backendFeatures(composed).features['drepDirectory.browse'];
    expect(browse.available).toBe(false);
    expect(browse.blockedBy?.cause).toBe('deployment');
  });

  it('breaks no core feature on db-sync', () => {
    // `brokenCore` is a deployment banner, not a hidden tab. The backend must
    // not create one: everything it takes away here is `enhanced`.
    expect(backendFeatures(COMPOSED).brokenCore).toEqual([]);
    const features = backendFeatures(COMPOSED).features;
    expect(features['drepDirectory.browse'].available).toBe(true);
    expect(features['govActionList.browse'].available).toBe(true);
    expect(features['account.votingPower'].available).toBe(true);
  });
});

describe('the /system endpoints', () => {
  function systemService(): SystemService {
    return new SystemService(
      chain({
        system: {
          getCapabilities: () => Promise.resolve({ meta: {}, data: DBSYNC }),
        },
      }),
      passthroughCache(),
    );
  }

  it('serves the COMPOSED document, not the provider one', async () => {
    const document = await systemService().getCapabilities();
    expect(document.provider).toBe(BACKEND_PROVIDER_ID);
    expect(document.composedFrom).toEqual([
      { provider: 'dbsync', network: 'mainnet' },
    ]);
    // The raise is in the served document, not only in the unit under test.
    expect(
      readCapability(document.datasets, 'drep.identity.current').paging,
    ).toMatchObject({ total: 'exact' });
  });

  it('derives the feature set from the composed document', async () => {
    const features = await systemService().getFeatures();
    expect(features.schemaVersion).toBe(2);
    expect(features.provider).toBe(BACKEND_PROVIDER_ID);
    expect(features.sourceDigest).toMatch(/^sha256:[0-9a-f]{64}$/);
    expect(features.features['drepDirectory.browse'].available).toBe(true);
  });

  it('turns a provider failure into an HTTP error rather than an empty document', async () => {
    const service = new SystemService(
      chain({
        system: {
          getCapabilities: () =>
            Promise.reject(new ChainDataError('PROVIDER_UNAVAILABLE', 'down')),
        },
      }),
      passthroughCache(),
    );
    await expect(service.getCapabilities()).rejects.toMatchObject({
      status: 503,
    });
  });
});

/** Referenced so the RouteId import is used by the type checker, not erased. */
const _routeIdIsTyped: RouteId = 'governance.dreps.list';
void _routeIdIsTyped;
