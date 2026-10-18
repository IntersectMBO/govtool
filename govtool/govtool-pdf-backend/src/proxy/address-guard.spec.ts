import {
  isBlockedAddress,
  matchGovtoolPath,
  parseFetchableUrl,
  parseIPv6,
  rewriteIpfs,
} from './address-guard';

describe('isBlockedAddress (§9.2)', () => {
  it.each([
    '0.0.0.0',
    '0.1.2.3',
    '10.0.0.1',
    '10.255.255.255',
    '100.64.0.1',
    '100.127.255.254',
    '127.0.0.1',
    '127.9.9.9',
    '169.254.169.254',
    '172.16.0.1',
    '172.31.255.255',
    '192.0.0.8',
    '192.0.2.1',
    '192.88.99.1',
    '192.168.1.1',
    '198.18.0.1',
    '198.19.255.255',
    '198.51.100.7',
    '203.0.113.9',
    '224.0.0.1',
    '239.255.255.250',
    '240.0.0.1',
    '255.255.255.255',
  ])('blocks IPv4 %s', (a) => expect(isBlockedAddress(a)).toBe(true));

  it.each([
    '1.1.1.1',
    '8.8.8.8',
    '100.63.255.255',
    '100.128.0.0',
    '172.15.255.255',
    '172.32.0.0',
    '198.17.0.1',
    '223.255.255.255',
  ])('allows public IPv4 %s', (a) => expect(isBlockedAddress(a)).toBe(false));

  it.each([
    '::',
    '::1',
    'fe80::1',
    'fe80::1%eth0',
    'febf::1',
    'fc00::1',
    'fd12:3456::1',
    'ff02::1',
    '2001:db8::1',
    '3fff::1',
    '100::1',
    'fec0::1',
    // IPv4-mapped, -compatible, NAT64 and 6to4 forms of blocked IPv4.
    '::ffff:127.0.0.1',
    '::ffff:7f00:1',
    '::ffff:10.0.0.1',
    '::ffff:169.254.169.254',
    '::127.0.0.1',
    '::a00:1',
    '64:ff9b::a00:1',
    '64:ff9b::169.254.169.254',
    '2002:7f00:1::',
    '2002:c0a8:101::1',
  ])('blocks IPv6 %s', (a) => expect(isBlockedAddress(a)).toBe(true));

  it.each([
    '2606:4700:4700::1111',
    '2a00:1450:4001::200e',
    '::ffff:8.8.8.8',
    '64:ff9b::808:808',
    '2002:808:808::1',
  ])('allows public IPv6 %s', (a) => expect(isBlockedAddress(a)).toBe(false));

  it('treats a non-IP as blocked', () => {
    expect(isBlockedAddress('localhost')).toBe(true);
    expect(isBlockedAddress('')).toBe(true);
  });
});

describe('parseIPv6', () => {
  it('expands :: and dotted tails', () => {
    expect(parseIPv6('::1')).toEqual([...Array<number>(15).fill(0), 1]);
    expect(parseIPv6('[::ffff:1.2.3.4]')).toEqual([...Array<number>(10).fill(0), 0xff, 0xff, 1, 2, 3, 4]);
    expect(parseIPv6('1:2:3:4:5:6:7:8')).toEqual([0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7, 0, 8]);
    expect(parseIPv6('1.2.3.4')).toBeNull();
    expect(parseIPv6('zz::1')).toBeNull();
  });
});

describe('parseFetchableUrl', () => {
  it('accepts http(s) without userinfo', () => {
    expect(parseFetchableUrl('https://example.com/x')?.hostname).toBe('example.com');
    expect(parseFetchableUrl('http://1.2.3.4:8080/')?.port).toBe('8080');
  });
  it.each([
    'ftp://example.com',
    'file:///etc/passwd',
    'javascript:alert(1)',
    'gopher://x',
    'data:text/plain,hi',
    'https://user@example.com',
    'https://user:pw@example.com',
    'https://:pw@example.com',
    'not a url',
    '//example.com',
  ])('rejects %s', (u) => expect(parseFetchableUrl(u)).toBeNull());
  it('normalises numeric IPv4 hosts, so the guard sees the real address', () => {
    expect(parseFetchableUrl('http://0x7f.1/')?.hostname).toBe('127.0.0.1');
    expect(parseFetchableUrl('http://2130706433/')?.hostname).toBe('127.0.0.1');
    expect(parseFetchableUrl('http://017700000001/')?.hostname).toBe('127.0.0.1');
  });
});

describe('rewriteIpfs', () => {
  const gw = 'https://ipfs.io/ipfs';
  it('rewrites ipfs:// to the gateway', () => {
    expect(rewriteIpfs('ipfs://bafyCID', gw)).toBe('https://ipfs.io/ipfs/bafyCID');
    expect(rewriteIpfs('ipfs://QmAbC/dir/file.txt', `${gw}/`)).toBe(
      'https://ipfs.io/ipfs/QmAbC/dir/file.txt',
    );
    expect(rewriteIpfs('IPFS://bafy', gw)).toBe('https://ipfs.io/ipfs/bafy');
  });
  it('leaves other URLs and refuses an empty CID', () => {
    expect(rewriteIpfs('https://x.io/a', gw)).toBe('https://x.io/a');
    expect(rewriteIpfs('ipfs://', gw)).toBeNull();
  });
});

describe('matchGovtoolPath (§9.1)', () => {
  const allowed = ['proposal/enacted-details'];
  it('matches exactly after percent-decoding', () => {
    expect(matchGovtoolPath('proposal/enacted-details', allowed)).toBe('proposal/enacted-details');
    expect(matchGovtoolPath('%70roposal/enacted-details', allowed)).toBe('proposal/enacted-details');
    expect(matchGovtoolPath('proposal/enacted-details/', allowed)).toBeNull();
    expect(matchGovtoolPath('proposal', allowed)).toBeNull();
    expect(matchGovtoolPath('', allowed)).toBeNull();
  });
  it.each([
    'proposal/../proposal/enacted-details',
    '../proposal/enacted-details',
    'proposal/%2e%2e/x',
    'proposal/%2E./x',
    'proposal%2fenacted-details',
    'proposal%2Fenacted-details',
    'proposal//enacted-details',
    'proposal\\enacted-details',
    'proposal%5cenacted-details',
    'proposal/%25%32%65',
    '%E0%A4%A',
  ])('rejects %s', (p) => expect(matchGovtoolPath(p, [...allowed, p])).toBeNull());
});
