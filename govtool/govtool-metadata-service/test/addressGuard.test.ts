import { describe, it, expect, afterEach } from 'vitest';
import { classifyAddress } from '../src/helpers/addressGuard';
import { PositionIndex } from '../src/helpers/positions';
import { gatewayUrl, parseIpfsUrl } from '../src/helpers/ipfs';

describe('classifyAddress', () => {
  it.each([
    ['8.8.8.8'], ['1.1.1.1'], ['2606:4700:4700::1111'], ['::ffff:8.8.8.8'],
  ])('allows the public address %s', (address) => {
    expect(classifyAddress(address)).toEqual({ public: true });
  });

  it.each([
    ['0.0.0.0', 'unspecified'],
    ['10.0.0.5', 'private'],
    ['172.31.0.5', 'private'],
    ['192.168.1.1', 'private'],
    ['100.64.0.1', 'carrierGradeNat'],
    ['100.100.100.100', 'carrierGradeNat'],
    ['127.0.0.1', 'loopback'],
    ['169.254.169.254', 'linkLocal'],
    ['192.0.0.8', 'reserved'],
    ['192.0.2.10', 'reserved'],
    ['198.18.0.1', 'reserved'],
    ['198.51.100.1', 'reserved'],
    ['203.0.113.1', 'reserved'],
    ['224.0.0.1', 'multicast'],
    ['240.0.0.1', 'reserved'],
    ['255.255.255.255', 'broadcast'],
    ['::', 'unspecified'],
    ['::1', 'loopback'],
    ['fe80::1', 'linkLocal'],
    ['fc00::1', 'uniqueLocal'],
    ['fd7a:115c:a1e0::1', 'uniqueLocal'],
    ['ff02::1', 'multicast'],
    ['2001:db8::1', 'reserved'],
    ['64:ff9b::a00:1', 'rfc6052'],
    ['2002:a00:1::', '6to4'],
    ['::ffff:10.0.0.1', 'private'],
    ['::ffff:169.254.169.254', 'linkLocal'],
    ['not-an-ip', 'invalid'],
  ])('refuses %s as %s', (address, range) => {
    expect(classifyAddress(address)).toEqual({ public: false, range });
  });
});

describe('PositionIndex', () => {
  it('maps offsets to 1-based line and column, and bytes separately', () => {
    const index = new PositionIndex('{\n  "é": 1\n}');
    expect(index.at(0)).toEqual({ offset: 0, byteOffset: 0, line: 1, column: 1 });
    // "é" is one UTF-16 unit but two UTF-8 bytes, so bytes run ahead after it.
    expect(index.at(5)).toEqual({ offset: 5, byteOffset: 5, line: 2, column: 4 });
    expect(index.at(6)).toEqual({ offset: 6, byteOffset: 7, line: 2, column: 5 });
    expect(index.at(11)).toEqual({ offset: 11, byteOffset: 12, line: 3, column: 1 });
  });

  it('converts a parser line and column, counting code points', () => {
    const text = '{"a": "😀", x}';
    const range = new PositionIndex(text).rangeAtLineColumn(1, 11)!;
    expect(text.slice(range.start.offset, range.end.offset)).toBe(' ');
  });

  it('finds the value at a JSON path, tolerating comments and trailing commas', () => {
    const text = '{ /* c */ "body": { "givenName": 42, }, }';
    const range = new PositionIndex(text).rangeOfPath(['body', 'givenName'])!;
    expect(text.slice(range.start.offset, range.end.offset)).toBe('42');
  });

  it('gives no path range for JSON5 it cannot parse', () => {
    expect(new PositionIndex("{ unquoted: 'x' }").rangeOfPath(['unquoted'])).toBeUndefined();
  });
});

describe('parseIpfsUrl', () => {
  const v1 = 'bafkrei' + 'a'.repeat(52);
  const v0 = 'Qm' + 'x'.repeat(44);

  it.each([
    [`ipfs://${v1}`, { namespace: 'ipfs', id: v1, rest: '' }],
    [`ipfs://${v0}/docs/drep.json`, { namespace: 'ipfs', id: v0, rest: '/docs/drep.json' }],
    ['ipns://example.org/meta.json', { namespace: 'ipns', id: 'example.org', rest: '/meta.json' }],
    [`https://ipfs.io/ipfs/${v0}`, { namespace: 'ipfs', id: v0, rest: '' }],
    [`https://most-brass-sun.quicknode-ipfs.com/ipfs/${v1}/a.json?x=1`, { namespace: 'ipfs', id: v1, rest: '/a.json?x=1' }],
    [`https://${v1}.ipfs.dweb.link/`, { namespace: 'ipfs', id: v1, rest: '' }],
    [`https://${v1}.ipfs.dweb.link/sub/doc.json`, { namespace: 'ipfs', id: v1, rest: '/sub/doc.json' }],
  ])('recognises %s', (url, expected) => {
    expect(parseIpfsUrl(url)).toEqual(expected);
  });

  it.each([
    ['https://example.com/drep.json'],
    ['https://example.com/ipfs/not-a-cid'],
    ['https://raw.githubusercontent.com/user/repo/main/ipfs/doc.json'],
    ['ipfs://not-a-cid'],
    ['not a url'],
  ])('leaves %s alone', (url) => {
    expect(parseIpfsUrl(url)).toBeUndefined();
  });

  it('builds a gateway url, keeping the path', () => {
    expect(gatewayUrl('https://gw.example/', { namespace: 'ipfs', id: v1, rest: '/a.json' }))
      .toBe(`https://gw.example/ipfs/${v1}/a.json`);
  });
});

describe('METADATA_ALLOW_PRIVATE_ADDRESSES (local testing only, D137)', () => {
  afterEach(() => {
    delete process.env.METADATA_ALLOW_PRIVATE_ADDRESSES;
  });

  it.each([['127.0.0.1'], ['172.31.0.3'], ['::1'], ['169.254.169.254']])(
    'allows %s when set to true',
    (address) => {
      process.env.METADATA_ALLOW_PRIVATE_ADDRESSES = 'true';
      expect(classifyAddress(address)).toEqual({ public: true });
    },
  );

  it.each([[undefined], ['false'], ['1'], ['']])('keeps blocking when set to %s', (value) => {
    if (value !== undefined) process.env.METADATA_ALLOW_PRIVATE_ADDRESSES = value;
    expect(classifyAddress('127.0.0.1')).toEqual({ public: false, range: 'loopback' });
  });
});
