import type {
  ChainDataApiV1,
  Envelope,
  GovAction,
  Page,
  ProviderCapabilities,
  ProviderHealth,
} from '@govtool/data-providers/chain-data';

import { CacheService } from 'src/cache/cache.service';
import { CacheWarmerService } from 'src/cache/cache-warmer.service';
import type { ConfigService } from 'src/config/config.service';
import { DRepService } from 'src/drep/drep.service';
import { ProposalService } from 'src/proposal/proposal.service';

const env = <T>(data: T): Envelope<T> => ({
  data,
  meta: { provider: 'stub', network: 'preview' },
});

const CAPABILITIES: ProviderCapabilities = {
  sorts: {
    dreps: ['registrationDate', 'random'],
    proposals: ['newest', 'oldest'],
  },
  filters: { dreps: [], proposals: [] },
  search: ['exactId'],
  voteAggregate: ['stake'],
  optionalArguments: [],
};

/** Typed like the contract, so a stub cannot return a shape no provider could. */
type StubApi = {
  system: Pick<ChainDataApiV1['system'], 'getHealth' | 'getCapabilities'>;
  governance: {
    dreps: Pick<ChainDataApiV1['governance']['dreps'], 'list'>;
    proposals: Pick<ChainDataApiV1['governance']['proposals'], 'list'>;
  };
};

function chain(stub: StubApi): ChainDataApiV1 {
  return stub as ChainDataApiV1;
}

function cache(): CacheService {
  const config = {
    get: () => ({
      cacheDurationSeconds: 0,
      drepListCacheDurationSeconds: 0,
      cacheMaxEntries: 1_000,
    }),
  } as unknown as ConfigService;
  return new CacheService(config);
}

/** Lets pending promise callbacks run. */
const settle = async () => {
  for (let i = 0; i < 20; i += 1) await Promise.resolve();
};

describe('CacheWarmerService', () => {
  beforeEach(() => jest.useFakeTimers());
  afterEach(() => jest.useRealTimers());

  it('does not start a second proposal snapshot while one is still running', async () => {
    let block = 100;
    let proposalCalls = 0;
    let finishProposals: (() => void) | undefined;
    const emptyPage = env<Page<GovAction>>({ elements: [], total: 0 });

    const api = chain({
      system: {
        // A new block on every tick, so every tick wants a refresh.
        getHealth: () =>
          Promise.resolve(
            env<ProviderHealth>({
              status: 'healthy',
              tip: { epoch: 1, block: block++ },
            }),
          ),
        getCapabilities: () => Promise.resolve(env(CAPABILITIES)),
      },
      governance: {
        // The DRep warm fails at once, as it did against Blockfrost.
        dreps: {
          list: () => Promise.reject(new Error('DRep directory unavailable')),
        },
        proposals: {
          list: () => {
            proposalCalls += 1;
            // The startup refresh completes; later ones hang until released.
            if (proposalCalls === 1) return Promise.resolve(emptyPage);
            return new Promise((resolve) => {
              finishProposals = () => resolve(emptyPage);
            });
          },
        },
      },
    });
    const store = cache();
    const proposals = new ProposalService(api, store, null);
    const warmer = new CacheWarmerService(
      api,
      store,
      new DRepService(api, proposals, store, null),
      proposals,
    );
    jest.spyOn(warmer['logger'], 'error').mockImplementation(() => undefined);
    jest.spyOn(warmer['logger'], 'log').mockImplementation(() => undefined);

    await warmer.onModuleInit();
    expect(proposalCalls).toBe(1);

    // Tick 1 starts a proposal snapshot that hangs; the DRep warm has already failed.
    jest.advanceTimersByTime(20_000);
    await settle();
    expect(proposalCalls).toBe(2);

    // Ticks 2 and 3 arrive while it is still running: no second copy.
    jest.advanceTimersByTime(20_000);
    await settle();
    jest.advanceTimersByTime(20_000);
    await settle();
    expect(proposalCalls).toBe(2);

    // Once it settles, the next tick may refresh again.
    finishProposals?.();
    await settle();
    jest.advanceTimersByTime(20_000);
    await settle();
    expect(proposalCalls).toBe(3);

    warmer.onModuleDestroy();
  });
});
