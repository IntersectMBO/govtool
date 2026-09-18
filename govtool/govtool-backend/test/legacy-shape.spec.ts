/**
 * Wire-compatibility tests.
 *
 * The backend's contract with the world is the legacy GovTool API — the shape
 * the Haskell backend served and the frontend still reads. These specs drive
 * each service with a stubbed `ChainDataApiV1` and assert the response body
 * exactly, key for key, so a change in the data layer that would alter the
 * wire format fails here rather than in the browser.
 *
 * `toEqual` is deliberate throughout: an extra or missing key is a break.
 */
import type {
  ChainDataApiV1,
  DRep,
  DRepListQuery,
  Delegation,
  Envelope,
  GovAction,
  PagedEnvelope,
  PredefinedDelegation,
  VotingPower,
} from '@govtool/data-providers/chain-data';
import { ChainDataError } from '@govtool/data-providers/chain-data';

import { AccountService } from '../src/account/account.service';
import { AdaHolderService } from '../src/ada-holder/ada-holder.service';
import { CacheService } from '../src/cache/cache.service';
import { ConfigService } from '../src/config/config.service';
import { DRepService } from '../src/drep/drep.service';
import { EpochService } from '../src/epoch/epoch.service';
import { NetworkService } from '../src/network/network.service';
import { ProposalService } from '../src/proposal/proposal.service';
import { TransactionService } from '../src/transaction/transaction.service';

const HASH = 'a'.repeat(56);
const TX = 'd'.repeat(64);

/** A cache that never caches, so each test sees the stub it set up. */
function passthroughCache(): CacheService {
  const config = {
    get: () => ({
      cacheDurationSeconds: 0,
      drepListCacheDurationSeconds: 0,
      cacheMaxEntries: 1_000,
    }),
  } as unknown as ConfigService;
  const cache = new CacheService(config);
  return cache;
}

/**
 * A partial provider for these tests.
 *
 * Deliberately **not** `as unknown as ChainDataApiV1`. That cast switched the
 * type checker off at the one boundary it was most needed: a stub could
 * return `{ elements: [...] }` with no `nextCursor`, which is not a valid
 * `Page`, and nothing complained. Every stub then modelled db-sync — one
 * complete page, always — so the suite could not see that Koios caps a page
 * at 1,000 rows and Blockfrost at 25. That is how the snapshot-truncation bug
 * reached live code with 35 specs passing.
 *
 * Each namespace is `Partial<…>`, so a test still stubs only the methods it
 * needs, but every stubbed method must match the contract's real signature —
 * including its return shape.
 */
type StubApi = {
  network?: Partial<ChainDataApiV1['network']>;
  accounts?: Partial<ChainDataApiV1['accounts']>;
  transactions?: Partial<ChainDataApiV1['transactions']>;
  system?: Partial<ChainDataApiV1['system']>;
  surveys?: Partial<NonNullable<ChainDataApiV1['surveys']>>;
  governance?: {
    dreps?: Partial<ChainDataApiV1['governance']['dreps']>;
    proposals?: Partial<ChainDataApiV1['governance']['proposals']>;
    votes?: Partial<ChainDataApiV1['governance']['votes']>;
    pools?: Partial<ChainDataApiV1['governance']['pools']>;
    committee?: Partial<ChainDataApiV1['governance']['committee']>;
    metrics?: Partial<ChainDataApiV1['governance']['metrics']>;
  };
};

function chain(overrides: StubApi): ChainDataApiV1 {
  return overrides as ChainDataApiV1;
}

beforeEach(() => {
  jest.restoreAllMocks();
});

describe('GET /account/:stakeKey', () => {
  it('returns the legacy body, with the internal row id as a number', async () => {
    const service = new AccountService(
      chain({
        accounts: {
          get: () =>
            Promise.resolve({
              meta: {},
              data: {
                stakeAddress: 'stake_test1abc',
                stakeKeyHash: HASH,
                isRegistered: true,
                isScriptBased: false,
                providerId: '9876',
              },
            }),
        },
      }),
      passthroughCache(),
    );

    await expect(service.getAccountInfo(HASH)).resolves.toEqual({
      id: 9876,
      view: 'stake_test1abc',
      isRegistered: true,
      isScriptBased: false,
    });
  });
});

describe('GET /ada-holder/get-current-delegation/:stakeKey', () => {
  function service(delegation: Delegation | null): AdaHolderService {
    return new AdaHolderService(
      chain({
        accounts: {
          getDelegation: () => Promise.resolve({ meta: {}, data: delegation }),
        },
      }),
      passthroughCache(),
    );
  }

  it('reports a real DRep using the pre-CIP-129 view, as the legacy field did', async () => {
    await expect(
      service({
        target: {
          kind: 'drep',
          drep: {
            role: 'drep',
            id: 'drep1cip129form',
            hash: 'b'.repeat(56),
            isScriptBased: true,
            cip105Id: 'drep1legacyview',
          },
        },
        txRef: { txHash: 'c'.repeat(64) },
      }).getCurrentDelegation(HASH),
    ).resolves.toEqual({
      drepHash: 'b'.repeat(56),
      drepView: 'drep1legacyview',
      isDRepScriptBased: true,
      txHash: 'c'.repeat(64),
    });
  });

  it.each<[PredefinedDelegation, string]>([
    ['alwaysAbstain', 'drep_always_abstain'],
    ['alwaysNoConfidence', 'drep_always_no_confidence'],
  ])(
    'maps the predefined target %s back to its db-sync view',
    async (option, view) => {
      await expect(
        service({
          target: { kind: 'predefined', option },
          txRef: { txHash: 'c'.repeat(64) },
        }).getCurrentDelegation(HASH),
      ).resolves.toEqual({
        drepHash: null,
        drepView: view,
        isDRepScriptBased: false,
        txHash: 'c'.repeat(64),
      });
    },
  );

  it('returns null when nothing is delegated', async () => {
    await expect(service(null).getCurrentDelegation(HASH)).resolves.toBeNull();
  });
});

describe('GET /ada-holder/get-voting-power/:stakeKey', () => {
  function service(
    result: () => Promise<Envelope<VotingPower | null>>,
  ): AdaHolderService {
    return new AdaHolderService(
      chain({ accounts: { getVotingPower: result } }),
      passthroughCache(),
    );
  }

  it('returns a bare number', async () => {
    await expect(
      service(() =>
        Promise.resolve({
          meta: {},
          data: { amount: '900000000', basis: 'live' },
        }),
      ).getVotingPower(HASH),
    ).resolves.toBe(900000000);
  });

  it('returns 0 for no record and 0 for a provider failure, as the legacy service did', async () => {
    await expect(
      service(() => Promise.resolve({ meta: {}, data: null })).getVotingPower(
        HASH,
      ),
    ).resolves.toBe(0);

    await expect(
      service(() =>
        Promise.reject(new ChainDataError('PROVIDER_UNAVAILABLE', 'down')),
      ).getVotingPower(HASH),
    ).resolves.toBe(0);
  });
});

describe('GET /network/*', () => {
  it('info carries the server clock plus the chain tip', async () => {
    const service = new NetworkService(
      chain({
        network: {
          getNetworkInfo: () =>
            Promise.resolve({
              meta: {},
              data: {
                network: 'preview',
                tip: { epoch: 500, block: 11_000_000 },
                epoch: { no: 500 },
              },
            }),
        },
      }),
      passthroughCache(),
    );

    const body = await service.getNetworkInfo();
    expect(Object.keys(body).sort()).toEqual([
      'blockNo',
      'currentTime',
      'epochNo',
      'networkName',
    ]);
    expect(body.epochNo).toBe(500);
    expect(body.blockNo).toBe(11_000_000);
    expect(body.networkName).toBe('preview');
    expect(Date.parse(body.currentTime)).not.toBeNaN();
  });

  it('total-stake keeps values above the safe range exact', async () => {
    const service = new NetworkService(
      chain({
        network: {
          getStakeDistribution: () =>
            Promise.resolve({
              meta: {},
              data: {
                totalStakeControlledByDReps: '31000000000000000',
                totalStakeControlledBySPOs: '22000000000000000',
                alwaysAbstainVotingPower: '4000000000000',
                alwaysNoConfidenceVotingPower: '900000000',
              },
            }),
        },
      }),
      passthroughCache(),
    );

    await expect(service.getNetworkTotalStake()).resolves.toEqual({
      // Both exceed Number.MAX_SAFE_INTEGER (9.0e15), so they stay bigint and
      // the response interceptor writes them unquoted. Rounding them to a
      // double would move the figure by thousands of ada.
      totalStakeControlledByDReps: 31000000000000000n,
      totalStakeControlledBySPOs: 22000000000000000n,
      alwaysAbstainVotingPower: 4000000000000,
      alwaysNoConfidenceVotingPower: 900000000,
    });
  });

  it('metrics returns all thirteen legacy keys', async () => {
    const service = new NetworkService(
      chain({
        governance: {
          metrics: {
            get: () =>
              Promise.resolve({
                meta: {},
                data: {
                  uniqueDelegators: 1000,
                  totalDelegations: 1200,
                  totalGovernanceActions: 50,
                  totalDRepVotes: 900,
                  totalRegisteredDReps: 700,
                  totalDRepDistribution: '31000000000000000',
                  totalActiveDReps: 400,
                  totalInactiveDReps: 300,
                  totalActiveCip119CompliantDReps: 250,
                  totalRegisteredDirectVoters: 20,
                  committee: {
                    size: 7,
                    quorum: { numerator: 2, denominator: 3 },
                  },
                },
              }),
          },
        },
      }),
      passthroughCache(),
    );

    await expect(service.getNetworkMetrics()).resolves.toEqual({
      uniqueDelegators: 1000,
      totalDelegations: 1200,
      totalGovernanceActions: 50,
      totalDRepVotes: 900,
      totalRegisteredDReps: 700,
      totalDRepDistr: 31000000000000000n,
      totalActiveDReps: 400,
      totalInactiveDReps: 300,
      totalActiveCIP119CompliantDReps: 250,
      totalRegisteredDirectVoters: 20,
      noOfCommitteeMembers: 7,
      quorumNumerator: 2,
      quorumDenominator: 3,
    });
  });
});

describe('GET /epoch/params', () => {
  it('returns db-sync’s epoch_param row verbatim, not the typed subset', async () => {
    const raw = { epoch_no: 500, drep_activity: 20, cost_model: null };
    const service = new EpochService(
      chain({
        network: {
          getProtocolParams: () =>
            Promise.resolve({
              meta: {},
              data: { epoch: 500, drepActivity: 20, raw },
            }),
        },
      }),
      passthroughCache(),
    );
    await expect(service.getCurrentEpochParams()).resolves.toBe(raw);
  });
});

describe('GET /transaction/status/:txId', () => {
  it('maps a confirmed transaction and its voting procedures', async () => {
    const service = new TransactionService(
      chain({
        transactions: {
          get: () =>
            Promise.resolve({
              meta: {},
              data: {
                txHash: TX,
                status: 'confirmed',
                votingProcedures: [{ id: 1 }],
              },
            }),
        },
      }),
    );
    await expect(service.getTransactionStatus(TX)).resolves.toEqual({
      transactionConfirmed: true,
      votingProcedure: [{ id: 1 }],
    });
  });

  it('reports an unindexed transaction as unconfirmed', async () => {
    const service = new TransactionService(
      chain({
        transactions: {
          get: () =>
            Promise.resolve({
              meta: {},
              data: { txHash: TX, status: 'unknown', votingProcedures: [] },
            }),
        },
      }),
    );
    await expect(service.getTransactionStatus(TX)).resolves.toEqual({
      transactionConfirmed: false,
      votingProcedure: [],
    });
  });
});

/* ------------------------------------------------------------------------- */
/* DReps                                                                      */
/* ------------------------------------------------------------------------- */

function fullDRep(overrides: Partial<DRep> = {}): DRep {
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
    registrationByKind: {
      drep: {
        isRegistered: true,
        wasRegistered: true,
        registrationTx: { txHash: 'c'.repeat(64) },
        retirementTx: null,
      },
      directVoter: {
        isRegistered: false,
        wasRegistered: false,
        registrationTx: null,
        retirementTx: null,
      },
    },
    metadata: {
      id: 'mid',
      anchor: { url: 'https://x/drep.jsonld', dataHash: 'b'.repeat(64) },
      standard: 'CIP119',
      status: 'valid',
      body: {
        givenName: 'Example DRep',
        objectives: 'Objectives',
        paymentAddress: 'stake1example',
        image: { url: 'https://x/i.png', contentHash: 'ee' },
        identityReferences: [
          { '@type': 'Identity', label: 'X', uri: 'https://x.com/e' },
        ],
        linkReferences: [],
      },
    },
    votingPower: { amount: '12500000000', basis: 'active' },
    activity: { votesCast: 7 },
    ...overrides,
  };
}

function drepService(chainStub: Record<string, unknown>): DRepService {
  const cache = passthroughCache();
  const api = chain(chainStub);
  const proposals = new ProposalService(api, cache);
  return new DRepService(api, proposals, cache);
}

describe('GET /drep/info/:drepId', () => {
  it('returns all twenty legacy keys, with the four registration booleans', async () => {
    const service = drepService({
      governance: {
        dreps: { get: () => Promise.resolve({ meta: {}, data: fullDRep() }) },
      },
    });

    await expect(service.getInfo(HASH)).resolves.toEqual({
      isScriptBased: false,
      isRegisteredAsDRep: true,
      wasRegisteredAsDRep: true,
      isRegisteredAsSoleVoter: false,
      wasRegisteredAsSoleVoter: false,
      deposit: 500000000,
      url: 'https://x/drep.jsonld',
      dataHash: 'b'.repeat(64),
      votingPower: 12500000000,
      dRepRegisterTxHash: 'c'.repeat(64),
      dRepRetireTxHash: null,
      soleVoterRegisterTxHash: null,
      soleVoterRetireTxHash: null,
      paymentAddress: 'stake1example',
      givenName: 'Example DRep',
      objectives: 'Objectives',
      motivations: null,
      qualifications: null,
      imageUrl: 'https://x/i.png',
      imageHash: 'ee',
    });
  });

  it('returns the all-empty record for an unregistered credential', async () => {
    const service = drepService({
      governance: {
        dreps: {
          get: () =>
            Promise.reject(new ChainDataError('NOT_FOUND', 'DRep not found')),
        },
      },
    });

    const body = await service.getInfo(HASH);
    expect(body).toEqual({
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
    });
  });

  it('propagates a real failure instead of hiding it as an empty record', async () => {
    const service = drepService({
      governance: {
        dreps: {
          get: () =>
            Promise.reject(new ChainDataError('PROVIDER_UNAVAILABLE', 'down')),
        },
      },
    });
    await expect(service.getInfo(HASH)).rejects.toMatchObject({ status: 503 });
  });
});

describe('GET /drep/list', () => {
  it('returns the legacy page envelope and item shape', async () => {
    const service = drepService({
      governance: {
        dreps: {
          list: () =>
            Promise.resolve({
              meta: {},
              data: { nextCursor: null, elements: [fullDRep()] },
            }),
        },
      },
    });

    const body = await service.list({ status: [], page: 0, pageSize: 10 });
    expect(body.page).toBe(0);
    expect(body.pageSize).toBe(10);
    expect(body.total).toBe(1);
    expect(body.elements[0]).toEqual({
      isScriptBased: false,
      // the legacy id is the raw hex hash, not a bech32 id
      drepId: HASH,
      view: 'drep1legacyview',
      url: 'https://x/drep.jsonld',
      metadataHash: 'b'.repeat(64),
      deposit: 500000000,
      votingPower: 12500000000,
      status: 'Active',
      type: 'DRep',
      latestTxHash: 'c'.repeat(64),
      latestRegistrationDate: '2026-01-01T00:00:00.000Z',
      metadataError: null,
      paymentAddress: 'stake1example',
      givenName: 'Example DRep',
      objectives: 'Objectives',
      motivations: null,
      qualifications: null,
      imageUrl: 'https://x/i.png',
      imageHash: 'ee',
      votesLastYear: 7,
      identityReferences: [
        { '@type': 'Identity', label: 'X', uri: 'https://x.com/e' },
      ],
      linkReferences: [],
    });
  });

  it('reports the reference arrays as [] for a DRep with no anchor, not null', async () => {
    // `list-dreps.sql` COALESCEs them to `[]` regardless of the anchor, so
    // the legacy field is never null. Caught by diffing against backend-ts
    // on live preview data.
    const service = drepService({
      governance: {
        dreps: {
          list: () =>
            Promise.resolve({
              meta: {},
              data: {
                nextCursor: null,
                elements: [fullDRep({ metadata: null })],
              },
            }),
        },
      },
    });

    const body = await service.list({ status: [], page: 0, pageSize: 10 });
    expect(body.elements[0]).toMatchObject({
      url: null,
      metadataHash: null,
      givenName: null,
      identityReferences: [],
      linkReferences: [],
    });
  });

  it('uses the legacy TitleCase status and type values', async () => {
    const service = drepService({
      governance: {
        dreps: {
          list: () =>
            Promise.resolve({
              meta: {},
              data: {
                nextCursor: null,
                elements: [
                  fullDRep({
                    registration: { status: 'inactive', deposit: '0' },
                  }),
                  fullDRep({
                    hash: 'b'.repeat(56),
                    kind: 'directVoter',
                    registration: { status: 'retired', deposit: '-1' },
                  }),
                ],
              },
            }),
        },
      },
    });

    // search '' hides direct voters, so ask with a search term matching one
    const all = await service.list({
      status: [],
      page: 0,
      pageSize: 10,
      search: 'b'.repeat(56),
    });
    expect(all.elements.map((e) => [e.status, e.type])).toEqual([
      ['Inactive', 'DRep'],
      ['Retired', 'SoleVoter'],
    ]);
  });

  it('hides direct voters with no search term, and reveals one on an exact id', async () => {
    const elements = [
      fullDRep(),
      fullDRep({ hash: 'b'.repeat(56), kind: 'directVoter' }),
    ];
    const service = drepService({
      governance: {
        dreps: {
          list: () => Promise.resolve({ meta: {}, data: { elements } }),
        },
      },
    });

    const bare = await service.list({ status: [], page: 0, pageSize: 10 });
    expect(bare.total).toBe(1);
    expect(bare.elements[0].type).toBe('DRep');

    const exact = await service.list({
      status: [],
      page: 0,
      pageSize: 10,
      search: 'b'.repeat(56),
    });
    expect(exact.total).toBe(2);
  });

  it('filters on the legacy status values and pages the result', async () => {
    const elements = Array.from({ length: 5 }, (_, i) =>
      fullDRep({ hash: String(i).repeat(56) }),
    );
    const service = drepService({
      governance: {
        dreps: {
          list: () => Promise.resolve({ meta: {}, data: { elements } }),
        },
      },
    });

    await expect(
      service.list({ status: ['Active'], page: 0, pageSize: 2 }),
    ).resolves.toMatchObject({ total: 5, page: 0, pageSize: 2 });

    // `sort` defaults to 'Random' (seeded), so paging is asserted with an
    // explicit deterministic order instead of insertion order.
    const paged = async (page: number) =>
      (
        await service.list({
          status: [],
          page,
          pageSize: 2,
          sort: 'RegistrationDate',
        })
      ).elements.map((e) => e.drepId);

    const [first, second, third] = [
      await paged(0),
      await paged(1),
      await paged(2),
    ];
    expect(second).toHaveLength(2);
    expect(third).toHaveLength(1);
    // the three pages partition the set with no overlap
    expect(new Set([...first, ...second, ...third]).size).toBe(5);

    await expect(
      service.list({ status: ['Retired'], page: 0, pageSize: 10 }),
    ).resolves.toMatchObject({ total: 0 });
  });
});

describe('GET /drep/list against a provider that pages', () => {
  /**
   * The regression the suite could not see before.
   *
   * `/drep/list` is served from a snapshot: fetch the whole directory, then
   * filter, sort and page in memory. Every other stub here returns one
   * complete page, which is db-sync's behaviour — so the suite passed while
   * the backend silently used the first page as the whole set. Koios caps a
   * page at 1,000 rows of 1,684 and Blockfrost at 25; both say so through
   * `nextCursor`, and the backend ignored it.
   */
  function pagingProvider(total: number, pageSize: number) {
    const calls: (string | undefined)[] = [];
    const list = (q?: DRepListQuery): Promise<PagedEnvelope<DRep>> => {
      calls.push(q?.cursor);
      const offset = q?.cursor === undefined ? 0 : Number(q.cursor);
      const elements = Array.from(
        { length: Math.min(pageSize, total - offset) },
        (_, i) => fullDRep({ hash: String(offset + i).padStart(56, '0') }),
      );
      const end = offset + elements.length;
      return Promise.resolve({
        data: {
          elements,
          nextCursor: end < total ? String(end) : null,
          total,
        },
        meta: {},
      });
    };
    return { calls, list };
  }

  it('pages the whole directory in, instead of taking the first page', async () => {
    const { calls, list } = pagingProvider(1684, 1000);
    const service = drepService({ governance: { dreps: { list } } });

    const body = await service.list({ status: [], page: 0, pageSize: 10 });

    // 1,684 DReps, not 1,000 — and `total` is the real total.
    expect(body.total).toBe(1684);
    expect(calls).toEqual([undefined, '1000']);
  });

  it('is unchanged for a provider that returns everything at once', async () => {
    const { calls, list } = pagingProvider(9, 1000);
    const service = drepService({ governance: { dreps: { list } } });

    await expect(
      service.list({ status: [], page: 0, pageSize: 10 }),
    ).resolves.toMatchObject({ total: 9 });
    expect(calls).toEqual([undefined]);
  });

  it('pages a Blockfrost-sized page too', async () => {
    const { calls, list } = pagingProvider(80, 25);
    const service = drepService({ governance: { dreps: { list } } });

    await expect(
      service.list({ status: [], page: 0, pageSize: 10 }),
    ).resolves.toMatchObject({ total: 80 });
    expect(calls).toHaveLength(4);
  });
});

describe('GET /drep/voting-power-list and /drep/get-voting-power/:drepId', () => {
  it('returns the four legacy keys per entry', async () => {
    const service = drepService({
      governance: {
        dreps: {
          getVotingPowers: () =>
            Promise.resolve({
              meta: {},
              data: [
                {
                  subject: {
                    kind: 'drep',
                    drep: {
                      role: 'drep',
                      id: 'drep1cip129',
                      hash: HASH,
                      isScriptBased: false,
                      cip105Id: 'drep1legacyview',
                    },
                  },
                  votingPower: { amount: '5000', basis: 'active' },
                  givenName: 'A',
                },
                {
                  subject: {
                    kind: 'drep',
                    drep: {
                      role: 'drep',
                      id: 'drep1other',
                      hash: 'b'.repeat(56),
                      isScriptBased: true,
                      cip105Id: 'drep_script1x',
                    },
                  },
                  votingPower: null,
                  givenName: null,
                },
              ],
            }),
        },
      },
    });

    await expect(service.getVotingPowerList([])).resolves.toEqual([
      {
        view: 'drep1legacyview',
        hashRaw: HASH,
        votingPower: 5000,
        givenName: 'A',
      },
      {
        view: 'drep_script1x',
        hashRaw: 'b'.repeat(56),
        votingPower: 0,
        givenName: null,
      },
    ]);
  });

  it('reports a predefined option with a null hash, as the legacy endpoint did', async () => {
    const service = drepService({
      governance: {
        dreps: {
          getVotingPowers: () =>
            Promise.resolve({
              meta: {},
              data: [
                {
                  subject: {
                    kind: 'predefined',
                    option: 'alwaysNoConfidence',
                    view: 'drep_always_no_confidence',
                  },
                  votingPower: { amount: '3707653134137', basis: 'active' },
                  givenName: null,
                },
              ],
            }),
        },
      },
    });

    await expect(service.getVotingPowerList([])).resolves.toEqual([
      {
        view: 'drep_always_no_confidence',
        hashRaw: null,
        votingPower: 3707653134137,
        givenName: null,
      },
    ]);
  });

  it('returns a bare number, and 0 when there is no distribution row', async () => {
    const withPower = drepService({
      governance: {
        dreps: {
          getVotingPower: () =>
            Promise.resolve({
              meta: {},
              data: [{ amount: '777', basis: 'active' }],
            }),
        },
      },
    });
    await expect(withPower.getVotingPower(HASH)).resolves.toBe(777);

    const none = drepService({
      governance: {
        dreps: {
          getVotingPower: () => Promise.resolve({ meta: {}, data: [] }),
        },
      },
    });
    await expect(none.getVotingPower(HASH)).resolves.toBe(0);
  });
});

/* ------------------------------------------------------------------------- */
/* Proposals                                                                  */
/* ------------------------------------------------------------------------- */

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
    metadata: {
      id: 'mid',
      anchor: { url: 'https://x/ga.jsonld', dataHash: 'e'.repeat(64) },
      standard: 'CIP108',
      status: 'valid',
      body: { title: 'A title', abstract: 'An abstract', authors: [] },
      raw: { body: {} },
    },
    tallies: [
      {
        role: 'drep',
        stake: { yes: '1000000', no: '2000000', abstain: '3000000' },
      },
      { role: 'spo', stake: { yes: '4000000', no: '0', abstain: '0' } },
      { role: 'cc', count: { yes: 3, no: 1, abstain: 0 } },
    ],
    ...overrides,
  };
}

function proposalService(chainStub: Record<string, unknown>): ProposalService {
  return new ProposalService(chain(chainStub), passthroughCache());
}

describe('GET /proposal/list', () => {
  it('returns every legacy key with the legacy value types', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          list: () =>
            Promise.resolve({
              meta: {},
              data: { nextCursor: null, elements: [govAction()] },
            }),
        },
      },
    });

    const body = await service.list({ type: [], page: 0, pageSize: 10 });
    expect(body.total).toBe(1);
    expect(body.elements[0]).toEqual({
      id: '42',
      txHash: TX,
      index: 0,
      type: 'InfoAction',
      details: { data: {} },
      expiryDate: '2026-03-01T00:00:00.000Z',
      expiryEpochNo: 510,
      createdDate: '2026-01-05T00:00:00.000Z',
      createdEpochNo: 500,
      url: 'https://x/ga.jsonld',
      metadataHash: 'e'.repeat(64),
      protocolParams: null,
      title: 'A title',
      abstract: 'An abstract',
      motivation: null,
      rationale: null,
      dRepYesVotes: 1000000,
      dRepNoVotes: 2000000,
      dRepAbstainVotes: 3000000,
      poolYesVotes: 4000000,
      poolNoVotes: 0,
      poolAbstainVotes: 0,
      ccYesVotes: 3,
      ccNoVotes: 1,
      ccAbstainVotes: 0,
      prevGovActionIndex: null,
      prevGovActionTxHash: null,
      json: { body: {} },
      authors: [],
    });
  });

  it('reports UpdateCommittee under db-sync’s name, which is what clients expect', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          list: () =>
            Promise.resolve({
              meta: {},
              data: {
                nextCursor: null,
                elements: [govAction({ type: 'UpdateCommittee' })],
              },
            }),
        },
      },
    });

    const body = await service.list({ type: [], page: 0, pageSize: 10 });
    // the contract renamed it; the wire format must not
    expect(body.elements[0].type).toBe('NewCommittee');
  });

  it('puts ParameterChange values on protocolParams', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          list: () =>
            Promise.resolve({
              meta: {},
              data: {
                nextCursor: null,
                elements: [
                  govAction({
                    type: 'ParameterChange',
                    body: {
                      type: 'ParameterChange',
                      changes: { drep_deposit: 500 },
                    },
                  }),
                ],
              },
            }),
        },
      },
    });
    const body = await service.list({ type: [], page: 0, pageSize: 10 });
    expect(body.elements[0].protocolParams).toEqual({ drep_deposit: 500 });
  });

  it('leaves authors as an empty array when the action has no anchor', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          list: () =>
            Promise.resolve({
              meta: {},
              data: {
                nextCursor: null,
                elements: [govAction({ metadata: null })],
              },
            }),
        },
      },
    });
    const body = await service.list({ type: [], page: 0, pageSize: 10 });
    expect(body.elements[0]).toMatchObject({
      url: null,
      metadataHash: null,
      title: null,
      json: null,
      authors: [],
    });
  });

  it('filters by the legacy type name and pages the result', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          list: () =>
            Promise.resolve({
              meta: {},
              data: {
                nextCursor: null,
                elements: [
                  govAction(),
                  govAction({
                    txHash: 'b'.repeat(64),
                    type: 'UpdateCommittee',
                    providerId: '43',
                  }),
                ],
              },
            }),
        },
      },
    });

    await expect(
      service.list({ type: ['NewCommittee'], page: 0, pageSize: 10 }),
    ).resolves.toMatchObject({ total: 1 });
    await expect(
      service.list({ type: [], page: 1, pageSize: 1 }),
    ).resolves.toMatchObject({ total: 2, page: 1, pageSize: 1 });
  });
});

describe('legacy ids under a provider with no internal row ids', () => {
  // Regression: db-sync exposes a numeric row id, which the contract keeps
  // opaque on `providerId`. Koios and Blockfrost have none, and the backend
  // used to emit the literal string "undefined" into the response.
  it('falls back to the CIP-129 action id for /proposal/list', async () => {
    const withoutProviderId = govAction();
    delete (withoutProviderId as { providerId?: string }).providerId;

    const service = proposalService({
      governance: {
        proposals: {
          list: () =>
            Promise.resolve({
              meta: {},
              data: { elements: [withoutProviderId], nextCursor: null },
            }),
        },
      },
    });

    const body = await service.list({ type: [], page: 0, pageSize: 10 });
    expect(body.elements[0].id).toBe(withoutProviderId.id);
    expect(body.elements[0].id).not.toContain('undefined');
  });

  it('still prefers the provider row id when there is one', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          list: () =>
            Promise.resolve({
              meta: {},
              data: { elements: [govAction()], nextCursor: null },
            }),
        },
      },
    });
    const body = await service.list({ type: [], page: 0, pageSize: 10 });
    expect(body.elements[0].id).toBe('42');
  });

  it('reports a missing enacted row id as null rather than NaN', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          getEnacted: () =>
            Promise.resolve({
              meta: {},
              data: {
                type: 'ParameterChange' as const,
                action: { id: 'gov_action1x', txHash: TX, index: 1 },
                submittedTx: { txHash: TX },
              },
            }),
        },
      },
    });

    await expect(
      service.getEnactedDetails('ParameterChange'),
    ).resolves.toMatchObject({ id: null, txId: null });
  });
});

describe('GET /proposal/get/:proposalId', () => {
  it('wraps the proposal with a null vote, as the legacy endpoint did', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          get: () => Promise.resolve({ meta: {}, data: govAction() }),
        },
      },
    });

    const body = await service.get(`${TX}#0`);
    expect(Object.keys(body).sort()).toEqual(['proposal', 'vote']);
    expect(body.vote).toBeNull();
    expect(body.proposal.id).toBe('42');
  });

  it('404s with the legacy message when the action is not live', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          get: () => Promise.reject(new ChainDataError('NOT_FOUND', 'nope')),
        },
      },
    });

    await expect(service.get(`${TX}#0`)).rejects.toMatchObject({
      status: 404,
      response: {
        errorType: 'NotFoundError',
        message: `Proposal with id: ${TX}#0 not found`,
      },
    });
  });
});

describe('GET /proposal/enacted-details', () => {
  it('returns the five legacy keys with numeric ids', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          getEnacted: () =>
            Promise.resolve({
              meta: {},
              data: {
                type: 'ParameterChange',
                action: {
                  id: 'gov_action1x',
                  txHash: TX,
                  index: 1,
                  providerId: '7',
                },
                submittedTx: { txHash: TX, providerId: '99' },
                rawBody: { tag: 'x' },
              },
            }),
        },
      },
    });

    await expect(service.getEnactedDetails('ParameterChange')).resolves.toEqual(
      {
        id: 7,
        txId: 99,
        index: 1,
        description: { tag: 'x' },
        hash: TX,
      },
    );
  });

  it('substitutes HardForkInitiation for any other type, as the legacy endpoint did', async () => {
    const requestedTypes: string[] = [];
    const getEnacted = (type: string) => {
      requestedTypes.push(type);
      return Promise.resolve({ meta: {}, data: null });
    };
    const service = proposalService({
      governance: { proposals: { getEnacted } },
    });

    await service.getEnactedDetails('InfoAction');
    await service.getEnactedDetails(undefined);
    expect(requestedTypes).toEqual([
      'HardForkInitiation',
      'HardForkInitiation',
    ]);
  });
});

describe('GET /drep/getVotes/:drepId', () => {
  it('returns the legacy vote/proposal pair shape', async () => {
    const service = drepService({
      governance: {
        dreps: {
          listVotes: () =>
            Promise.resolve({
              meta: {},
              data: {
                nextCursor: null,
                elements: [
                  {
                    vote: {
                      proposal: {
                        id: 'gov_action1abc',
                        txHash: TX,
                        index: 0,
                        providerId: '42',
                      },
                      voter: {
                        role: 'drep',
                        id: 'drep1cip129',
                        hash: HASH,
                        isScriptBased: false,
                      },
                      vote: 'yes',
                      txRef: { txHash: '9'.repeat(64) },
                      at: { epoch: 501, time: '2026-01-10T00:00:00.000Z' },
                      votingPower: null,
                      rationale: {
                        id: 'rid',
                        anchor: {
                          url: 'https://x/r.jsonld',
                          dataHash: 'f'.repeat(64),
                        },
                        standard: 'CIP100',
                        status: 'pending',
                      },
                      isCurrent: true,
                    },
                    proposal: govAction(),
                  },
                ],
              },
            }),
        },
      },
    });

    const votes = await service.getVotes(HASH);
    expect(votes).toHaveLength(1);
    expect(votes[0].vote).toEqual({
      proposalId: '42',
      drepId: HASH,
      vote: 'yes',
      url: 'https://x/r.jsonld',
      metadataHash: 'f'.repeat(64),
      epochNo: 501,
      date: '2026-01-10T00:00:00.000Z',
      txHash: '9'.repeat(64),
    });
    expect(votes[0].proposal.id).toBe('42');
    expect(votes[0].proposal.type).toBe('InfoAction');
  });

  it('filters the pairs by proposal type', async () => {
    const pair = (type: string, txHash: string) => ({
      vote: {
        proposal: { id: 'x', txHash, index: 0, providerId: '1' },
        voter: { role: 'drep', id: 'd', hash: HASH, isScriptBased: false },
        vote: 'no',
        txRef: { txHash: '9'.repeat(64) },
        at: { epoch: 1, time: '2026-01-10T00:00:00.000Z' },
        votingPower: null,
        rationale: null,
        isCurrent: true,
      },
      proposal: govAction({ txHash, type: type as GovAction['type'] }),
    });

    const service = drepService({
      governance: {
        dreps: {
          listVotes: () =>
            Promise.resolve({
              meta: {},
              data: {
                nextCursor: null,
                elements: [
                  pair('InfoAction', TX),
                  pair('UpdateCommittee', 'b'.repeat(64)),
                ],
              },
            }),
        },
      },
    });

    // filtering uses the legacy type name
    await expect(
      service.getVotes(HASH, ['NewCommittee']),
    ).resolves.toHaveLength(1);
    await expect(service.getVotes(HASH, [])).resolves.toHaveLength(2);
  });
});
