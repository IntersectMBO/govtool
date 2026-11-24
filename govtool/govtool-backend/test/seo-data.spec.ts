import type {
  ChainDataApiV1,
  DRep,
  Envelope,
  GovAction,
  NetworkInfo,
  PagedEnvelope,
  ProviderCapabilities,
} from '@govtool/data-providers/chain-data';
import type {
  MetadataResult,
  MetadataServiceV1,
} from '@govtool/data-providers/metadata';

import { CacheService } from '../src/cache/cache.service';
import { LegacyNetwork } from '../src/common/legacy-network';
import type { ConfigService } from '../src/config/config.service';
import { DRepService } from '../src/drep/drep.service';
import { GovernanceActionsService } from '../src/governance-actions/governance-actions.service';
import type { MetadataService } from '../src/metadata/metadata.service';
import { ProposalService } from '../src/proposal/proposal.service';
import { ForumClient } from '../src/seo/forum.client';
import type { SystemService } from '../src/system/system.service';
import { actionId, drepId } from './ids';

const META = { provider: 'stub', network: 'mainnet' } as const;
const env = <T>(data: T): Envelope<T> => ({ data, meta: { ...META } });
const page = <T>(elements: T[]): PagedEnvelope<T> =>
  env({ elements, total: elements.length });

/** Typed per namespace, never cast whole: see legacy-shape.spec.ts. */
type StubApi = {
  network?: Partial<ChainDataApiV1['network']>;
  system?: Partial<ChainDataApiV1['system']>;
  governance?: {
    proposals?: Partial<ChainDataApiV1['governance']['proposals']>;
    dreps?: Partial<ChainDataApiV1['governance']['dreps']>;
  };
};

function chain(overrides: StubApi): ChainDataApiV1 {
  return overrides as ChainDataApiV1;
}

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

const networkInfo: NetworkInfo = {
  network: 'mainnet',
  era: 'conway',
  tip: { epoch: 510 },
  currentEpoch: 510,
};

/** A metadata service answering each hash with its body, counting calls. */
function documents(bodies: Record<string, Record<string, unknown>>) {
  const calls: string[] = [];
  const service = {
    getMetadata: (hash: string): Promise<MetadataResult> => {
      calls.push(hash);
      const body = bodies[hash];
      return Promise.resolve(
        body === undefined
          ? {
              ok: false,
              code: 'HASH_MISMATCH',
              category: 'INVALID_CONTENT',
              message: 'mismatch',
              checkedAt: '2026-01-01T00:00:00Z',
            }
          : {
              ok: true,
              hash,
              fetchedAt: '2026-01-01T00:00:00Z',
              body: { body },
            },
      );
    },
  } as unknown as MetadataServiceV1;
  return { calls, service };
}

/* ------------------------------------------------------------------------- */
/* Governance actions                                                         */
/* ------------------------------------------------------------------------- */

const TX = 'd'.repeat(64);
const TX2 = 'c'.repeat(64);
const HASH_A = 'a'.repeat(64);

function govAction(overrides: Partial<GovAction> = {}): GovAction {
  return {
    id: actionId(TX, 0),
    txHash: TX,
    index: 0,
    type: 'InfoAction',
    body: { type: 'InfoAction' },
    lifecycle: {
      status: 'live',
      submitted: { epoch: 505, time: '2026-02-04T00:00:00.000Z' },
      submittedTx: { txHash: TX, index: 0 },
      expires: { epoch: 511 },
      ratifiedAt: null,
      enactedAt: null,
      droppedAt: null,
      expiredAt: null,
    },
    anchor: { url: 'https://x/a.jsonld', dataHash: HASH_A },
    deposit: '100000000000',
    depositReturnAddress: 'stake1example',
    previousAction: null,
    voteAggregates: [],
    ...overrides,
  };
}

function actionsService(actions: GovAction[], metadata: MetadataServiceV1) {
  const api = chain({
    network: { getNetworkInfo: () => Promise.resolve(env(networkInfo)) },
    governance: {
      proposals: { list: () => Promise.resolve(page(actions)) },
    },
  });
  const cache = passthroughCache();
  const proposals = new ProposalService(
    api,
    cache,
    metadata,
    new LegacyNetwork(api),
  );
  return new GovernanceActionsService(
    api,
    metadata,
    proposals,
    cache,
    new LegacyNetwork(api),
    {} as MetadataService,
    {} as SystemService,
    { get: () => ({ pdfApiUrl: null }) } as unknown as ConfigService,
  );
}

describe('GovernanceActionsService.storedSeoAction', () => {
  const titled = { title: 'Fund the tooling', abstract: 'A plan.' };

  it('reads the stored title and never fetches while a request waits', async () => {
    const { calls, service } = documents({ [HASH_A]: titled });
    const svc = actionsService([govAction()], service);

    const before = await svc.storedSeoAction(actionId(TX, 0));
    expect(before).toMatchObject({ cip129Id: actionId(TX, 0), title: null });
    expect(calls).toEqual([]);

    await svc['proposals'].warmDocuments();
    const after = await svc.storedSeoAction(actionId(TX, 0));
    expect(after).toMatchObject({
      cip129Id: actionId(TX, 0),
      type: 'InfoAction',
      title: 'Fund the tooling',
      abstract: 'A plan.',
      expiryEpoch: 511,
      createdDate: '2026-02-04T00:00:00.000Z',
      status: {
        ratified_epoch: null,
        enacted_epoch: null,
        dropped_epoch: null,
        expired_epoch: null,
      },
    });
    expect(calls).toEqual([HASH_A]);
  });

  it('resolves a bare tx hash only when it names one action', async () => {
    const { service } = documents({});
    const two = actionsService(
      [
        govAction(),
        govAction({ id: actionId(TX, 1), index: 1 }),
        govAction({ id: actionId(TX2, 0), txHash: TX2 }),
      ],
      service,
    );
    await expect(two.storedSeoAction(TX)).resolves.toBeNull();
    await expect(two.storedSeoAction(TX2)).resolves.toMatchObject({
      cip129Id: actionId(TX2, 0),
    });
  });

  it('answers null for an id it does not hold or cannot read', async () => {
    const { service } = documents({});
    const svc = actionsService([govAction()], service);
    await expect(svc.storedSeoAction(actionId(TX2, 3))).resolves.toBeNull();
    await expect(svc.storedSeoAction('gov_action1nope')).resolves.toBeNull();
  });

  it('lists every action for the sitemap', async () => {
    const { service } = documents({});
    const svc = actionsService(
      [govAction(), govAction({ id: actionId(TX2, 0), txHash: TX2 })],
      service,
    );
    const all = await svc.storedSeoActions();
    expect(all.map((a) => a.cip129Id).sort()).toEqual(
      [actionId(TX, 0), actionId(TX2, 0)].sort(),
    );
  });
});

/* ------------------------------------------------------------------------- */
/* DReps                                                                      */
/* ------------------------------------------------------------------------- */

const DREP_A = drepId('a');
const DREP_B = drepId('b');
const DREP_C = drepId('c');
const DOC_A = '1'.repeat(64);
const DOC_B = '2'.repeat(64);

function drep(id: string, overrides: Partial<DRep> = {}): DRep {
  return {
    role: 'drep',
    id,
    isScriptBased: false,
    kind: 'drep',
    anchor: { url: `https://x/${id}.jsonld`, dataHash: DOC_A },
    registration: {
      latest: {
        txRef: { txHash: 'e'.repeat(64) },
        at: { epoch: 500, time: '2026-01-01T00:00:00.000Z' },
        deposit: '500000000',
      },
      latestUpdate: null,
    },
    status: 'active',
    votingPower: { amount: '482774913502419', basis: 'active' },
    activity: { voted: 7, votable: 9 },
    ...overrides,
  };
}

function drepsService(dreps: DRep[], metadata: MetadataServiceV1) {
  const capabilities = {
    sorts: { dreps: ['registrationDate', 'random'], proposals: ['newest'] },
    filters: { dreps: [], proposals: [] },
    search: ['exactId'],
    voteAggregate: ['stake'],
    optionalArguments: [],
  } as unknown as ProviderCapabilities;
  const api = chain({
    network: { getNetworkInfo: () => Promise.resolve(env(networkInfo)) },
    system: { getCapabilities: () => Promise.resolve(env(capabilities)) },
    governance: {
      dreps: { list: () => Promise.resolve(page(dreps)) },
      proposals: { list: () => Promise.resolve(page([])) },
    },
  });
  const cache = passthroughCache();
  const proposals = new ProposalService(api, cache, metadata);
  return new DRepService(api, proposals, cache, metadata);
}

describe('DRepService.storedSeoDRep', () => {
  const dreps = [
    drep(DREP_A),
    drep(DREP_B, {
      anchor: { url: 'https://x/b.jsonld', dataHash: DOC_B },
      status: 'retired',
    }),
    drep(DREP_C, { anchor: null }),
  ];
  const bodies = {
    [DOC_A]: { givenName: 'YUTA', objectives: 'Bridge the gap.' },
    [DOC_B]: { givenName: 'Hidden', doNotList: true },
  };

  it('reads the stored profile and never fetches while a request waits', async () => {
    const { calls, service } = documents(bodies);
    const svc = drepsService(dreps, service);

    const before = await svc.storedSeoDRep(DREP_A);
    expect(before).toMatchObject({ givenName: null, hasMetadata: true });
    expect(calls).toEqual([]);

    await svc.warmDocuments();
    const after = await svc.storedSeoDRep(DREP_A);
    expect(after).toMatchObject({
      givenName: 'YUTA',
      objectives: 'Bridge the gap.',
      status: 'Active',
      votingPower: 482774913502419n,
      doNotList: false,
      metadataError: null,
      hasMetadata: true,
      latestRegistrationDate: '2026-01-01T00:00:00.000Z',
    });
  });

  it('carries doNotList, retirement and a missing anchor', async () => {
    const { service } = documents(bodies);
    const svc = drepsService(dreps, service);
    await svc.warmDocuments();

    await expect(svc.storedSeoDRep(DREP_B)).resolves.toMatchObject({
      doNotList: true,
      status: 'Retired',
    });
    await expect(svc.storedSeoDRep(DREP_C)).resolves.toMatchObject({
      hasMetadata: false,
    });
  });

  it('answers null for a DRep it does not hold', async () => {
    const { service } = documents(bodies);
    const svc = drepsService(dreps, service);
    await expect(svc.storedSeoDRep(drepId('f'))).resolves.toBeNull();
    await expect(svc.storedSeoDRep('drep1nope')).resolves.toBeNull();
  });

  it('lists every DRep for the sitemap', async () => {
    const { service } = documents(bodies);
    const svc = drepsService(dreps, service);
    const all = await svc.storedSeoDReps();
    expect(all).toHaveLength(3);
  });
});

/* ------------------------------------------------------------------------- */
/* Forum proposals                                                            */
/* ------------------------------------------------------------------------- */

describe('ForumClient.proposal', () => {
  const forumConfig = (pdfApiUrl: string | null) =>
    ({ get: () => ({ pdfApiUrl }) }) as unknown as ConfigService;
  afterEach(() => jest.restoreAllMocks());

  it('answers null when no forum is configured', async () => {
    const fetchSpy = jest.spyOn(global, 'fetch');
    const client = new ForumClient(forumConfig(null));
    await expect(client.proposal('42')).resolves.toBeNull();
    expect(fetchSpy).not.toHaveBeenCalled();
  });

  it('answers undefined at once on a miss and the proposal once fetched', async () => {
    const fetchSpy = jest.spyOn(global, 'fetch').mockResolvedValue(
      new Response(
        JSON.stringify({
          data: {
            id: 42,
            attributes: {
              updatedAt: '2026-03-01T00:00:00.000Z',
              content: {
                id: 7,
                attributes: {
                  prop_name: 'Fund a dev rel team',
                  prop_abstract: 'Twelve months.',
                },
              },
            },
          },
          meta: {},
        }),
        { status: 200, headers: { 'content-type': 'application/json' } },
      ),
    );
    const client = new ForumClient(forumConfig('http://pdf:1337/api'));

    await expect(client.proposal('42')).resolves.toBeUndefined();
    await client.settled();
    await expect(client.proposal('42')).resolves.toEqual({
      id: '42',
      title: 'Fund a dev rel team',
      abstract: 'Twelve months.',
      updatedAt: '2026-03-01T00:00:00.000Z',
    });
    expect(fetchSpy).toHaveBeenCalledTimes(1);
    expect(fetchSpy.mock.calls[0][0]).toBe('http://pdf:1337/api/proposals/42');
  });

  it('answers null for a proposal without content or one the forum does not have', async () => {
    jest
      .spyOn(global, 'fetch')
      .mockResolvedValueOnce(
        new Response(
          JSON.stringify({ data: { id: 5, attributes: { content: null } } }),
          { status: 200 },
        ),
      )
      .mockResolvedValueOnce(
        new Response('Proposal not found', { status: 400 }),
      )
      .mockResolvedValueOnce(new Response('', { status: 404 }));
    const client = new ForumClient(forumConfig('http://pdf:1337/api'));

    await client.proposal('5');
    await client.proposal('6');
    await client.proposal('7');
    await client.settled();
    await expect(client.proposal('5')).resolves.toBeNull();
    await expect(client.proposal('6')).resolves.toBeNull();
    await expect(client.proposal('7')).resolves.toBeNull();
  });

  it('never answers null for a forum that is down or slow', async () => {
    jest
      .spyOn(global, 'fetch')
      .mockResolvedValueOnce(new Response('', { status: 502 }))
      .mockRejectedValueOnce(
        new Error('The operation was aborted due to timeout'),
      );
    const client = new ForumClient(forumConfig('http://pdf:1337/api'));

    await client.proposal('8');
    await client.proposal('9');
    await client.settled();
    // Not known: generic, indexable wording, and asked again next time.
    await expect(client.proposal('8')).resolves.toBeUndefined();
    await expect(client.proposal('9')).resolves.toBeUndefined();
  });

  it('keeps a proposal it has when a refresh fails', async () => {
    const proposal = {
      data: {
        id: 42,
        attributes: {
          updatedAt: '2026-03-01T00:00:00.000Z',
          content: {
            id: 7,
            attributes: { prop_name: 'Kept', prop_abstract: null },
          },
        },
      },
    };
    const fetchSpy = jest
      .spyOn(global, 'fetch')
      .mockResolvedValueOnce(
        new Response(JSON.stringify(proposal), { status: 200 }),
      )
      .mockResolvedValueOnce(new Response('', { status: 503 }));
    const now = jest.spyOn(Date, 'now');
    const client = new ForumClient(forumConfig('http://pdf:1337/api'));

    now.mockReturnValue(1_000);
    await client.proposal('42');
    await client.settled();
    // Past the five-minute lifetime: a refresh starts, and fails.
    now.mockReturnValue(1_000 + 301_000);
    await client.proposal('42');
    await client.settled();
    expect(fetchSpy).toHaveBeenCalledTimes(2);
    await expect(client.proposal('42')).resolves.toMatchObject({
      title: 'Kept',
    });
  });
});
