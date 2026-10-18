import { redactIp, redactIpsDeep, redactIpsInText } from './redact-ip';

describe('redactIp', () => {
  it.each([
    ['203.0.113.45', '203.xx.xx.45'],
    ['10.0.0.5', '10.xx.xx.5'],
    ['2606:4700::6810:84e5', '2606:xx:xx:84e5'],
    ['2001:db8:0:0:0:0:0:1', '2001:xx:xx:1'],
    ['::1', '0:xx:xx:1'],
    ['fe80::', 'fe80:xx:xx:0'],
    ['::ffff:192.168.1.20', '::ffff:192.xx.xx.20'],
  ])('redacts %s as %s', (address, redacted) => {
    expect(redactIp(address)).toBe(redacted);
  });

  it.each(['example.org', '', '999.1.1.1', 'not:an:address'])(
    'leaves %p alone',
    (value) => {
      expect(redactIp(value)).toBe(value);
    },
  );
});

describe('redactIpsInText', () => {
  it.each([
    [
      'connect ECONNREFUSED 172.31.0.5:5432',
      'connect ECONNREFUSED 172.xx.xx.5:5432',
    ],
    [
      'Refused: example.org resolves only to non-public addresses: 10.0.0.5 (private), 127.0.0.1 (loopback)',
      'Refused: example.org resolves only to non-public addresses: 10.xx.xx.5 (private), 127.xx.xx.1 (loopback)',
    ],
    [
      'Timeout(10sec) during connect [timeout 2606:4700::6810:84e5] (2 addresses tried)',
      'Timeout(10sec) during connect [timeout 2606:xx:xx:84e5] (2 addresses tried)',
    ],
    [
      'connect ECONNREFUSED ::ffff:10.1.2.3:443',
      'connect ECONNREFUSED ::ffff:10.xx.xx.3:443',
    ],
    ['blocked 10.0.0.5.', 'blocked 10.xx.xx.5.'],
  ])('redacts %p', (text, redacted) => {
    expect(redactIpsInText(text)).toBe(redacted);
  });

  it.each([
    'no such host',
    'checked at 2026-09-24T10:00:00.000Z',
    'retry at 10:00:00.',
    'version 1.2.3.4.5',
    'hash ' + 'ab'.repeat(32),
    'Error: ENOTFOUND',
  ])('leaves %p alone', (text) => {
    expect(redactIpsInText(text)).toBe(text);
  });
});

describe('redactIpsDeep', () => {
  it('redacts every string except under the kept keys, without mutating', () => {
    const input = {
      url: 'http://10.0.0.5/doc.json',
      message: 'refused 10.0.0.5',
      hops: [{ dns: { addresses: [{ address: '10.0.0.5', family: 4 }] } }],
      body: { data: '{"ip":"10.0.0.5"}' },
    };
    const copy = structuredClone(input);

    expect(redactIpsDeep(input, new Set(['url', 'body']))).toEqual({
      url: 'http://10.0.0.5/doc.json',
      message: 'refused 10.xx.xx.5',
      hops: [{ dns: { addresses: [{ address: '10.xx.xx.5', family: 4 }] } }],
      body: { data: '{"ip":"10.0.0.5"}' },
    });
    expect(input).toEqual(copy);
  });
});
