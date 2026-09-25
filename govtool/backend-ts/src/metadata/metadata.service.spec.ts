import * as blake from 'blakejs';
import { MetadataService } from './metadata.service';
import { ConfigService } from '../config/config.service';
import { fetchMetadataText } from './safe-metadata-fetch';
import { MetadataValidationStatus } from './metadata-status.enum';
jest.mock('./safe-metadata-fetch', () => ({
  ...jest.requireActual<typeof import('./safe-metadata-fetch')>(
    './safe-metadata-fetch',
  ),
  fetchMetadataText: jest.fn(),
}));

describe('metadata IPFS configuration', () => {
  it('uses the configured gateway and sends credentials only for IPFS URLs', async () => {
    const service = new MetadataService({
      get: () => ({
        ipfsGateway: 'https://example.org/ipfs/',
        ipfsProjectId: 'test-project',
      }),
    } as ConfigService);
    const raw = '{"body":{"givenName":"Example"},"standard":"CIP-119"}';
    jest.mocked(fetchMetadataText).mockResolvedValue(raw);
    const hash = blake.blake2bHex(raw, undefined, 32);
    await service.validateMetadata({ url: 'ipfs://cid', hash });
    expect(fetchMetadataText).toHaveBeenLastCalledWith(
      'https://example.org/ipfs/cid',
      expect.objectContaining({ project_id: 'test-project' }),
    );
    await service.validateMetadata({ url: 'https://example.net/data', hash });
    expect(jest.mocked(fetchMetadataText).mock.lastCall![1]).not.toHaveProperty(
      'project_id',
    );
  });
});

describe('CIP-108 title and abstract validation', () => {
  const service = new MetadataService({
    get: () => ({}),
  } as ConfigService);

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
    ['title over 80 chars', { title: 'a'.repeat(81), abstract: 'Abstract' }],
    [
      'abstract over 2500 chars',
      { title: 'Title', abstract: 'b'.repeat(2501) },
    ],
  ])('rejects %s', async (_case, body) => {
    const result = await validate(body);
    expect(result).toMatchObject({
      valid: false,
      status: MetadataValidationStatus.INCORRECT_FORMAT,
    });
  });
});
