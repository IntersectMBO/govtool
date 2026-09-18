import * as blake from 'blakejs';
import { MetadataService } from './metadata.service';
import { ConfigService } from '../config/config.service';
import { fetchMetadataText } from './safe-metadata-fetch';
jest.mock('./safe-metadata-fetch', () => ({ fetchMetadataText: jest.fn() }));

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
