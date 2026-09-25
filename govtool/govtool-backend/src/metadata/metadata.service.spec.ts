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
  ): MetadataServiceV1 => ({
    getMetadata,
    getCipMetadata: () => Promise.reject(new Error('unused')),
    refresh: () => Promise.reject(new Error('unused')),
    getReport: () => Promise.resolve(null),
    listReports: () => Promise.resolve([]),
  });
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
});
