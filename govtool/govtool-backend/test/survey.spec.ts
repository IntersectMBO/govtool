import type {
  ChainDataApiV1,
  Envelope,
  ProviderCapabilities,
  SurveyDefinition,
} from '@govtool/data-providers/chain-data';
import { ChainDataError } from '@govtool/data-providers/chain-data';
import type { MetadataServiceV1 } from '@govtool/data-providers/metadata';
import type { Response } from 'express';

import { CacheService } from '../src/cache/cache.service';
import type { ConfigService } from '../src/config/config.service';
import { isAvailable } from '../src/system/capabilities';
import { ProposalService } from '../src/proposal/proposal.service';
import { SystemService } from '../src/system/system.service';
import { SurveyController } from '../src/survey/survey.controller';
import { SurveyService } from '../src/survey/survey.service';

const TX = 'ab'.repeat(32);
// {17: [0, []]}: a singleton label-17 map around an empty definitions batch.
const PAYLOAD = 'a111820080';

const env = <T>(data: T): Envelope<T> => ({
  data,
  meta: { provider: 'stub', network: 'preview' },
});

const definition = (txHash = TX): SurveyDefinition => ({
  txHash,
  metadataLabel: 17,
  payloadCborHex: PAYLOAD,
});

function cache(): CacheService {
  return new CacheService({
    get: () => ({
      cacheDurationSeconds: 0,
      drepListCacheDurationSeconds: 0,
      cacheMaxEntries: 1_000,
    }),
  } as unknown as ConfigService);
}

type GetDefinition = NonNullable<ChainDataApiV1['surveys']>['getDefinition'];

function service(getDefinition?: jest.Mock<ReturnType<GetDefinition>>) {
  const chain = (
    getDefinition === undefined ? {} : { surveys: { getDefinition } }
  ) as ChainDataApiV1;
  return new SurveyService(chain, cache());
}

const found = () =>
  jest.fn<ReturnType<GetDefinition>, Parameters<GetDefinition>>(() =>
    Promise.resolve(env(definition())),
  );

describe('GET /survey/definition/:txId/:index', () => {
  it('answers the legacy body with the whole label-17 batch', async () => {
    const getDefinition = found();
    await expect(
      service(getDefinition).getDefinition(TX.toUpperCase(), '0'),
    ).resolves.toEqual({
      txId: TX,
      surveyIndex: 0,
      metadataLabel: 17,
      payloadCborHex: PAYLOAD,
    });
    // Asked in the lowercase form the contract reports.
    expect(getDefinition).toHaveBeenCalledWith(TX);
  });

  it.each(['1', '65535'])(
    'returns the same batch for index %s; the frontend selects it',
    async (index) => {
      await expect(
        service(found()).getDefinition(TX, index),
      ).resolves.toMatchObject({
        surveyIndex: Number(index),
        payloadCborHex: PAYLOAD,
      });
    },
  );

  it.each([
    ['a short hash', 'ab', '0'],
    ['a non-hex hash', 'z'.repeat(64), '0'],
    ['an index past 65535', TX, '65536'],
    ['a negative index', TX, '-1'],
    ['a non-numeric index', TX, 'x'],
  ])('refuses %s with a 400 and no provider call', async (_, tx, index) => {
    const getDefinition = found();
    await expect(
      service(getDefinition).getDefinition(tx, index),
    ).rejects.toMatchObject({ status: 400 });
    expect(getDefinition).not.toHaveBeenCalled();
  });

  it('answers a transaction without label 17 with the legacy 404 body', async () => {
    const getDefinition = jest.fn(() => Promise.resolve(env(null)));
    const error = await service(getDefinition)
      .getDefinition(TX, '0')
      .catch((e: unknown) => e);
    expect(error).toMatchObject({ status: 404 });
    expect((error as { getResponse(): unknown }).getResponse()).toEqual({
      errorType: 'NotFoundError',
      message: `No metadata label 17 found for transaction ${TX}`,
    });
  });

  it('answers 501 when the provider serves no survey definitions', async () => {
    await expect(service().getDefinition(TX, '0')).rejects.toMatchObject({
      status: 501,
    });
  });

  it.each([
    ['PROVIDER_UNAVAILABLE', 503],
    ['PROVIDER_RATE_LIMITED', 429],
    ['PROVIDER_TIMEOUT', 504],
    ['INTERNAL', 500],
  ] as const)('maps %s to %i', async (code, status) => {
    const getDefinition = jest.fn(() =>
      Promise.reject(new ChainDataError(code, 'upstream')),
    );
    await expect(
      service(getDefinition).getDefinition(TX, '0'),
    ).rejects.toMatchObject({ status });
  });

  describe('caching', () => {
    afterEach(() => jest.useRealTimers());

    it('reads a transaction once for every index and either case', async () => {
      const getDefinition = found();
      const svc = service(getDefinition);
      await svc.getDefinition(TX, '0');
      await svc.getDefinition(TX.toUpperCase(), '3');
      expect(getDefinition).toHaveBeenCalledTimes(1);
    });

    it('shares one provider call between concurrent requests', async () => {
      let release: () => void = () => {};
      const getDefinition = jest.fn<
        ReturnType<GetDefinition>,
        Parameters<GetDefinition>
      >(
        () =>
          new Promise((resolve) => {
            release = () => resolve(env(definition()));
          }),
      );
      const svc = service(getDefinition);
      const both = Promise.all([
        svc.getDefinition(TX, '0'),
        svc.getDefinition(TX, '1'),
      ]);
      release();
      await both;
      expect(getDefinition).toHaveBeenCalledTimes(1);
    });

    it('keeps a read for 60 seconds, then asks again', async () => {
      jest.useFakeTimers();
      const getDefinition = found();
      const svc = service(getDefinition);
      await svc.getDefinition(TX, '0');
      jest.advanceTimersByTime(59_000);
      await svc.getDefinition(TX, '0');
      expect(getDefinition).toHaveBeenCalledTimes(1);
      jest.advanceTimersByTime(2_000);
      await svc.getDefinition(TX, '0');
      expect(getDefinition).toHaveBeenCalledTimes(2);
    });

    it('does not keep a failure: a later request recovers', async () => {
      const getDefinition = jest
        .fn<ReturnType<GetDefinition>, Parameters<GetDefinition>>()
        .mockResolvedValueOnce(env(null))
        .mockRejectedValueOnce(
          new ChainDataError('PROVIDER_UNAVAILABLE', 'down'),
        )
        .mockResolvedValue(env(definition()));
      const svc = service(getDefinition);
      await expect(svc.getDefinition(TX, '0')).rejects.toMatchObject({
        status: 404,
      });
      await expect(svc.getDefinition(TX, '0')).rejects.toMatchObject({
        status: 503,
      });
      await expect(svc.getDefinition(TX, '0')).resolves.toMatchObject({
        payloadCborHex: PAYLOAD,
      });
    });
  });

  describe('Cache-Control', () => {
    const response = () => {
      const headers: Record<string, string> = {};
      return {
        headers,
        res: {
          setHeader: (name: string, value: string) => {
            headers[name] = value;
          },
        } as unknown as Response,
      };
    };

    it('lets a definition be kept for 60 seconds', async () => {
      const { headers, res } = response();
      await new SurveyController(service(found())).getDefinition(TX, '0', res);
      expect(headers['Cache-Control']).toBe('public, max-age=60');
    });

    it('never lets an error be kept', async () => {
      const { headers, res } = response();
      const controller = new SurveyController(
        service(jest.fn(() => Promise.resolve(env(null)))),
      );
      await expect(controller.getDefinition(TX, '0', res)).rejects.toThrow();
      expect(headers['Cache-Control']).toBe('no-store');
    });
  });
});

describe('the survey.linkedVoting feature', () => {
  const capabilities: ProviderCapabilities = {
    sorts: { dreps: [], proposals: [] },
    filters: { dreps: [], proposals: [] },
    search: ['exactId'],
    voteAggregate: ['stake'],
    optionalArguments: [],
  };
  const features = (surveys: boolean) => {
    const chain = {
      system: { getCapabilities: () => Promise.resolve(env(capabilities)) },
      ...(surveys && { surveys: { getDefinition: found() } }),
    } as unknown as ChainDataApiV1;
    return new SystemService(chain, cache()).getFeatures();
  };

  it('is available when the provider has the surveys namespace', async () => {
    expect(isAvailable(await features(true), 'survey.linkedVoting')).toBe(true);
  });

  it('is unavailable, with no source, when it does not', async () => {
    const set = await features(false);
    expect(isAvailable(set, 'survey.linkedVoting')).toBe(false);
    expect(set.unavailable['survey.linkedVoting']?.cause).toBe('noSource');
  });
});

describe('a governance action links a survey only through its verified document', () => {
  const ACTION_TX = 'd'.repeat(64);
  const document = {
    body: {
      title: 'Fund it',
      cip179: { specVersion: 5, surveyRef: { txId: TX, index: 0 } },
    },
  };
  const action = {
    id: 'gov_action1stub',
    txHash: ACTION_TX,
    index: 0,
    type: 'InfoAction',
    body: { type: 'InfoAction' },
    lifecycle: {
      status: 'live',
      submitted: { epoch: 500, time: '2026-01-05T00:00:00.000Z' },
      submittedTx: { txHash: ACTION_TX, index: 0 },
      expires: { epoch: 510, time: '2026-03-01T00:00:00.000Z' },
      ratifiedAt: null,
      enactedAt: null,
      droppedAt: null,
      expiredAt: null,
    },
    anchor: { url: 'https://x/ga.jsonld', dataHash: 'e'.repeat(64) },
    deposit: null,
    depositReturnAddress: null,
    previousAction: null,
    voteAggregates: [],
  };
  const proposals = (
    result: Awaited<ReturnType<MetadataServiceV1['getMetadata']>>,
  ) =>
    new ProposalService(
      {
        governance: {
          proposals: {
            list: () =>
              Promise.resolve({
                data: { elements: [action], total: 1 },
                meta: { provider: 'stub', network: 'preview' },
              }),
          },
        },
      } as unknown as ChainDataApiV1,
      cache(),
      {
        getMetadata: () => Promise.resolve(result),
        getCipMetadata: () => Promise.reject(new Error('unused')),
        refresh: () => Promise.reject(new Error('unused')),
        getReport: () => Promise.resolve(null),
        listReports: () => Promise.resolve([]),
      },
    );

  it('carries body.cip179 in json when the document verifies', async () => {
    const service = proposals({
      ok: true,
      hash: 'e'.repeat(64),
      body: document,
      fetchedAt: '2026-10-02T00:00:00Z',
    });
    await service.warmDocuments();
    const body = await service.list({ type: [], page: 0, pageSize: 10 });
    expect(body.elements[0].json).toEqual(document);
  });

  it('carries no link when the document fails its hash check', async () => {
    const service = proposals({
      ok: false,
      code: 'HASH_MISMATCH',
      category: 'INVALID_CONTENT',
      message: 'Hash of fetched data does not match',
      servedHash: 'f'.repeat(64),
      checkedAt: '2026-10-02T00:00:00Z',
    });
    await service.warmDocuments();
    const body = await service.list({ type: [], page: 0, pageSize: 10 });
    expect(body.elements[0].json).toBeNull();
  });
});
