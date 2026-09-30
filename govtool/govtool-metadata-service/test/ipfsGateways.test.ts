import { describe, it, expect, afterEach } from 'vitest';
import { IPFS_GATEWAYS } from '../src/config';
import { gatewayOrder, setIpfsGatewaysForTesting } from '../src/helpers/ipfs';

describe('IPFS_GATEWAYS', () => {
  afterEach(() => {
    delete process.env.IPFS_GATEWAYS;
    delete process.env.IPFS_PRIMARY_GATEWAY;
    setIpfsGatewaysForTesting(undefined);
  });

  it('uses the default public list when unset', () => {
    expect([...gatewayOrder().order].sort()).toEqual([...IPFS_GATEWAYS].sort());
  });

  it('replaces the default list, so only local gateways are tried', () => {
    process.env.IPFS_GATEWAYS = ' http://test-metadata-api:3000/ , http://other:8080';
    process.env.IPFS_PRIMARY_GATEWAY = 'http://test-metadata-api:3000';
    expect(gatewayOrder().order).toEqual(['http://test-metadata-api:3000', 'http://other:8080']);
  });

  it('ignores entries that are not http(s) urls, and falls back when none are', () => {
    process.env.IPFS_GATEWAYS = 'file:///etc,http://ok:1';
    expect(gatewayOrder().order).toEqual(['http://ok:1']);
    process.env.IPFS_GATEWAYS = 'nonsense, ftp://x';
    expect([...gatewayOrder().order].sort()).toEqual([...IPFS_GATEWAYS].sort());
  });
});
