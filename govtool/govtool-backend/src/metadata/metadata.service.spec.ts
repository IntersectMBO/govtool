import type { MetadataServiceV1 } from '@govtool/data-providers/metadata';
import * as blake from 'blakejs';
import { MetadataService } from './metadata.service';
import { ConfigService } from '../config/config.service';
import { MetadataValidationStatus } from './metadata-status.enum';
import { MetadataStandard } from './metadata.type';
import { fetchMetadataText, MetadataFetchError } from './safe-metadata-fetch';
jest.mock('./safe-metadata-fetch', () => ({
  ...jest.requireActual<typeof import('./safe-metadata-fetch')>(
    './safe-metadata-fetch',
  ),
  fetchMetadataText: jest.fn(),
}));

describe('metadata IPFS configuration', () => {
  it('uses the configured gateway and sends credentials only for IPFS URLs', async () => {
    const service = new MetadataService(
      {
        get: () => ({
          ipfsGateway: 'https://example.org/ipfs/',
          ipfsProjectId: 'test-project',
          metadataAllowPrivateUrls: false,
        }),
      } as ConfigService,
      null,
    );
    const raw = '{"body":{"givenName":"Example"},"standard":"CIP-119"}';
    jest.mocked(fetchMetadataText).mockResolvedValue(raw);
    const hash = blake.blake2bHex(raw, undefined, 32);
    await service.validateMetadata({ url: 'ipfs://cid', hash });
    expect(fetchMetadataText).toHaveBeenLastCalledWith(
      'https://example.org/ipfs/cid',
      expect.objectContaining({ project_id: 'test-project' }),
      { allowPrivateAddresses: false },
    );
    await service.validateMetadata({ url: 'https://example.net/data', hash });
    expect(jest.mocked(fetchMetadataText).mock.lastCall![1]).not.toHaveProperty(
      'project_id',
    );
  });
});

describe('CIP100 vote rationale', () => {
  it('returns the comment the frontend shows for a vote rationale', async () => {
    const service = new MetadataService(
      {
        get: () => ({
          ipfsGateway: '',
          ipfsProjectId: '',
          metadataAllowPrivateUrls: false,
        }),
      } as ConfigService,
      null,
    );
    const raw = JSON.stringify({
      body: { comment: { '@value': 'Voting yes because...' } },
    });
    jest.mocked(fetchMetadataText).mockResolvedValue(raw);

    await expect(
      service.validateMetadata({
        url: 'https://example.org/rationale.jsonld',
        hash: blake.blake2bHex(raw, undefined, 32),
        standard: MetadataStandard.CIP100,
      }),
    ).resolves.toMatchObject({
      valid: true,
      metadata: { comment: 'Voting yes because...' },
    });
  });
});

describe('metadata size limit', () => {
  it('reports an oversize document as EXCEEDS_LIMIT, not URL_NOT_FOUND', async () => {
    const service = new MetadataService(
      {
        get: () => ({ ipfsGateway: '', ipfsProjectId: '' }),
      } as ConfigService,
      null,
    );
    jest
      .mocked(fetchMetadataText)
      .mockRejectedValue(
        new MetadataFetchError(MetadataValidationStatus.EXCEEDS_LIMIT),
      );
    await expect(
      service.validateMetadata({
        url: 'https://example.org/big.jsonld',
        hash: 'a'.repeat(64),
      }),
    ).resolves.toEqual({
      status: MetadataValidationStatus.EXCEEDS_LIMIT,
      valid: false,
      metadata: undefined,
    });
  });
});

describe('validation through the metadata service', () => {
  beforeEach(() => jest.clearAllMocks());

  const config = {
    get: () => ({ ipfsGateway: '', ipfsProjectId: '' }),
  } as ConfigService;
  const doc = {
    '@context': {
      CIP119:
        'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/README.md#',
    },
    body: { givenName: { '@value': 'HOSKY' } },
  };
  const stub = (
    getMetadata: MetadataServiceV1['getMetadata'],
    verify?: MetadataServiceV1['verify'],
  ): MetadataServiceV1 => ({
    getMetadata,
    getCipMetadata: () => Promise.reject(new Error('unused')),
    refresh: () => Promise.reject(new Error('unused')),
    ...(verify && { verify }),
    getReport: () => Promise.resolve(null),
    listReports: () => Promise.resolve([]),
  });
  const notFound = {
    ok: false,
    code: 'FETCH_ERROR',
    category: 'NETWORK',
    message: 'Unexpected Status code: 404',
    reportId: 'rep-1',
    checkedAt: '2026-10-02T00:00:00Z',
  } as const;
  const input = {
    url: 'https://ipfs.io/ipfs/QmaAAqY6zwaLRSoqDdQBAMRoYKjydxJfEkpwtSiCWKJCdi',
    hash: 'AB'.repeat(32),
  };

  it('uses the service result and does not fetch locally', async () => {
    const getMetadata = jest.fn<
      ReturnType<MetadataServiceV1['getMetadata']>,
      Parameters<MetadataServiceV1['getMetadata']>
    >(() =>
      Promise.resolve({
        ok: true,
        hash: 'ab'.repeat(32),
        body: doc,
        fetchedAt: '2026-09-24T00:00:00Z',
      }),
    );
    const service = new MetadataService(config, stub(getMetadata));
    const result = await service.validateMetadata(input);
    expect(getMetadata).toHaveBeenCalledWith('ab'.repeat(32), input.url);
    expect(result).toEqual({
      status: undefined,
      valid: true,
      metadata: { givenName: 'HOSKY' },
    });
    expect(fetchMetadataText).not.toHaveBeenCalled();
  });

  it('adds the document authors only when asked', async () => {
    const authors = [{ name: 'A', witness: { witnessAlgorithm: 'ed25519' } }];
    const service = new MetadataService(
      config,
      stub(() =>
        Promise.resolve({
          ok: true,
          hash: 'ab'.repeat(32),
          body: { ...doc, authors },
          fetchedAt: '2026-09-24T00:00:00Z',
        }),
      ),
    );
    await expect(
      service.validateMetadata(input, { includeAuthors: true }),
    ).resolves.toMatchObject({ metadata: { givenName: 'HOSKY', authors } });
    await expect(service.validateMetadata(input)).resolves.toMatchObject({
      metadata: { givenName: 'HOSKY' },
    });
    const plain = await service.validateMetadata(input);
    expect(plain.metadata).not.toHaveProperty('authors');
  });

  it.each([
    ['FETCH_ERROR', 'No IPFS gateway served the content', 'URL_NOT_FOUND'],
    [
      'FETCH_ERROR',
      'Refused: host resolves only to non-public addresses',
      'URL_BLOCKED',
    ],
    ['HASH_MISMATCH', 'Hash of fetched data does not match', 'INVALID_HASH'],
    ['EXCEEDS_LIMIT', 'too large', 'EXCEEDS_LIMIT'],
    ['JSON_PARSE_ERROR', 'Unable to parse data into JSON', 'INCORRECT_FORMAT'],
  ] as const)('maps %s (%s) to %s', async (code, message, status) => {
    const service = new MetadataService(
      config,
      stub(() =>
        Promise.resolve({
          ok: false,
          code,
          category: 'NETWORK',
          message,
          checkedAt: '2026-09-24T00:00:00Z',
        }),
      ),
    );
    await expect(service.validateMetadata(input)).resolves.toMatchObject({
      status,
      valid: false,
    });
  });

  it('falls back to the local fetch when the service is unreachable', async () => {
    (fetchMetadataText as jest.Mock).mockResolvedValueOnce(JSON.stringify(doc));
    const service = new MetadataService(
      config,
      stub(() => Promise.reject(new Error('connect ECONNREFUSED'))),
    );
    await service.validateMetadata(input);
    expect(fetchMetadataText).toHaveBeenCalled();
  });

  it('carries the reportId of a failure the service saw', async () => {
    const service = new MetadataService(
      config,
      stub(() => Promise.resolve(notFound)),
    );
    await expect(service.validateMetadata(input)).resolves.toEqual({
      status: 'URL_NOT_FOUND',
      valid: false,
      metadata: undefined,
      reportId: 'rep-1',
    });
  });

  it('with verifyUrl asks the service to verify, not the cache, and carries its report', async () => {
    const getMetadata = jest.fn<
      ReturnType<MetadataServiceV1['getMetadata']>,
      Parameters<MetadataServiceV1['getMetadata']>
    >();
    const verify = jest.fn<
      ReturnType<NonNullable<MetadataServiceV1['verify']>>,
      Parameters<NonNullable<MetadataServiceV1['verify']>>
    >(() => Promise.resolve(notFound));
    const service = new MetadataService(config, stub(getMetadata, verify));
    await expect(
      service.validateMetadata({ ...input, verifyUrl: true }),
    ).resolves.toMatchObject({
      status: 'URL_NOT_FOUND',
      valid: false,
      reportId: 'rep-1',
    });
    expect(verify).toHaveBeenCalledWith('ab'.repeat(32), input.url);
    expect(getMetadata).not.toHaveBeenCalled();
    expect(fetchMetadataText).not.toHaveBeenCalled();
  });

  it('with verifyUrl over the rate limit verifies with a local fetch instead', async () => {
    const raw = JSON.stringify(doc);
    const hash = blake.blake2bHex(raw, undefined, 32);
    const verify = jest.fn<
      ReturnType<NonNullable<MetadataServiceV1['verify']>>,
      Parameters<NonNullable<MetadataServiceV1['verify']>>
    >(() =>
      Promise.resolve({
        ok: true,
        hash,
        body: doc,
        fetchedAt: '2026-10-02T00:00:00Z',
      }),
    );
    jest.mocked(fetchMetadataText).mockResolvedValue(raw);
    const service = new MetadataService(
      config,
      stub(() => Promise.reject(new Error('unused')), verify),
    );
    for (let i = 0; i < 31; i++) {
      await service.validateMetadata(
        { ...input, hash, verifyUrl: true },
        { clientKey: 'client-a' },
      );
    }
    expect(verify).toHaveBeenCalledTimes(30);
    expect(fetchMetadataText).toHaveBeenCalledTimes(1);
  });

  it('with verifyUrl and no verify on the service fetches the url itself and never asks the cache', async () => {
    const raw = JSON.stringify(doc);
    jest.mocked(fetchMetadataText).mockResolvedValueOnce(raw);
    const getMetadata = jest.fn<
      ReturnType<MetadataServiceV1['getMetadata']>,
      Parameters<MetadataServiceV1['getMetadata']>
    >();
    const service = new MetadataService(config, stub(getMetadata));
    await expect(
      service.validateMetadata({
        ...input,
        hash: blake.blake2bHex(raw, undefined, 32),
        verifyUrl: true,
      }),
    ).resolves.toMatchObject({ valid: true });
    expect(getMetadata).not.toHaveBeenCalled();
    expect(fetchMetadataText).toHaveBeenCalledWith(
      input.url,
      expect.anything(),
      expect.anything(),
    );
  });

  it('with verifyUrl rejects a url that does not serve the document, even when the hash is cached', async () => {
    jest
      .mocked(fetchMetadataText)
      .mockRejectedValueOnce(
        new MetadataFetchError(MetadataValidationStatus.URL_NOT_FOUND),
      );
    const service = new MetadataService(
      config,
      stub(() =>
        Promise.resolve({
          ok: true,
          hash: 'ab'.repeat(32),
          body: doc,
          fetchedAt: '2026-09-24T00:00:00Z',
        }),
      ),
    );
    await expect(
      service.validateMetadata({ ...input, verifyUrl: true }),
    ).resolves.toMatchObject({ status: 'URL_NOT_FOUND', valid: false });
  });

  it('falls back to the local fetch when the service exceeds its budget', async () => {
    jest.useFakeTimers();
    try {
      jest.mocked(fetchMetadataText).mockResolvedValueOnce(JSON.stringify(doc));
      const service = new MetadataService(
        config,
        stub(() => new Promise(() => {})),
      );
      const pending = service.validateMetadata(input);
      await jest.advanceTimersByTimeAsync(15_000);
      await pending;
      expect(fetchMetadataText).toHaveBeenCalled();
    } finally {
      jest.useRealTimers();
    }
  });
});

describe('CIP-108 title and abstract validation', () => {
  const service = new MetadataService(
    {
      get: () => ({
        ipfsGateway: '',
        ipfsProjectId: '',
        metadataAllowPrivateUrls: false,
      }),
    } as ConfigService,
    null,
  );

  const validate = async (body: Record<string, unknown>) => {
    const raw = JSON.stringify({
      hashAlgorithm: 'blake2b-256',
      body: {
        motivation: 'Motivation',
        rationale: 'Rationale',
        ...body,
      },
      '@context': {
        CIP108:
          'https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#',
      },
    });
    jest.mocked(fetchMetadataText).mockResolvedValue(raw);
    const hash = blake.blake2bHex(raw, undefined, 32);
    return service.validateMetadata({ url: 'https://example.net/meta', hash });
  };

  it('accepts non-blank title and abstract', async () => {
    const result = await validate({ title: 'Title', abstract: 'Abstract' });
    expect(result).toMatchObject({ valid: true, status: undefined });
  });

  it('accepts title and abstract at the length limits', async () => {
    const result = await validate({
      title: 'a'.repeat(80),
      abstract: 'b'.repeat(2500),
    });
    expect(result.valid).toBe(true);
  });

  it.each([
    ['empty title', { title: '', abstract: 'Abstract' }],
    ['whitespace-only title', { title: '  \n\t ', abstract: 'Abstract' }],
    ['empty abstract', { title: 'Title', abstract: '' }],
    ['whitespace-only abstract', { title: 'Title', abstract: '   ' }],
    ['non-string title', { title: 42, abstract: 'Abstract' }],
    ['missing abstract', { title: 'Title' }],
  ])('rejects %s', async (_case, body) => {
    const result = await validate(body);
    expect(result).toMatchObject({
      valid: false,
      status: MetadataValidationStatus.INCORRECT_FORMAT,
    });
    expect(result.metadata).toBeUndefined();
  });

  it('names every missing field', async () => {
    const result = await validate({ title: 'Title', rationale: undefined });
    expect(result.issues).toEqual([
      { field: 'abstract', rule: 'required', severity: 'error' },
      { field: 'rationale', rule: 'required', severity: 'error' },
    ]);
  });

  it('accepts an over-long title and abstract with warnings and the content', async () => {
    const result = await validate({
      title: 'a'.repeat(84),
      abstract: 'b'.repeat(3000),
    });
    expect(result).toMatchObject({
      valid: true,
      status: undefined,
      metadata: { title: 'a'.repeat(84) },
    });
    expect(result.issues).toEqual([
      {
        field: 'title',
        rule: 'maxLength',
        severity: 'warning',
        limit: 80,
        actual: 84,
      },
      {
        field: 'abstract',
        rule: 'maxLength',
        severity: 'warning',
        limit: 2500,
        actual: 3000,
      },
    ]);
  });

  it('returns no issues for a document within the limits', async () => {
    const result = await validate({ title: 'Title', abstract: 'Abstract' });
    expect(result).not.toHaveProperty('issues');
  });
});

describe('CIP-119 validation', () => {
  it('names a missing givenName', async () => {
    const service = new MetadataService(
      {
        get: () => ({
          ipfsGateway: '',
          ipfsProjectId: '',
          metadataAllowPrivateUrls: false,
        }),
      } as ConfigService,
      null,
    );
    const raw = JSON.stringify({ body: { objectives: 'x' } });
    jest.mocked(fetchMetadataText).mockResolvedValue(raw);
    const hash = blake.blake2bHex(raw, undefined, 32);
    const result = await service.validateMetadata({
      url: 'https://example.net/drep',
      hash,
      standard: MetadataStandard.CIP119,
    });
    expect(result).toMatchObject({
      valid: false,
      status: MetadataValidationStatus.INCORRECT_FORMAT,
      issues: [{ field: 'givenName', rule: 'required', severity: 'error' }],
    });
  });
});
