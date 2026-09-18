import { ChainDataError } from '@govtool/data-providers/chain-data';

import { FakeKoios } from './fake-http';
import { KoiosHttpClient, parseContentRange } from '../src/http/client';
import { expectChainDataError } from './expect-error';

describe('KoiosHttpClient', () => {
  it('sends the bearer token and the count preference', async () => {
    const koios = new FakeKoios().on('tip', [{ epoch_no: 1 }]);
    await koios
      .client({ token: 'secret' })
      .get('tip', {}, { count: 'exact', limit: 10, offset: 20 });

    const call = koios.lastCallTo('tip')!;
    expect(call.headers['authorization']).toBe('Bearer secret');
    expect(call.headers['prefer']).toBe('count=exact');
    expect(call.params).toMatchObject({ limit: '10', offset: '20' });
  });

  it('omits offset=0 so the first page is a plain query', async () => {
    const koios = new FakeKoios().on('tip', []);
    await koios.client().get('tip', {}, { offset: 0, limit: 5 });
    expect(koios.lastCallTo('tip')!.params['offset']).toBeUndefined();
  });

  it('reads the row total out of content-range', async () => {
    const koios = new FakeKoios().on('vote_list', [{}, {}], { total: 4 });
    const response = await koios.client().get('vote_list');
    expect(response.total).toBe(4);
  });

  it('reports no total when Koios answers with a wildcard range', async () => {
    const koios = new FakeKoios().on('vote_list', [{}]);
    expect((await koios.client().get('vote_list')).total).toBeNull();
  });

  it('maps a rate limit to a retryable error carrying Retry-After', async () => {
    const koios = new FakeKoios().failOn('tip', 429, { 'retry-after': '30' });
    const error = await expectChainDataError(koios.client().get('tip'));

    expect(ChainDataError.is(error)).toBe(true);
    expect(error.code).toBe('PROVIDER_RATE_LIMITED');
    expect(error.retryable).toBe(true);
    expect(error.retryAfterSeconds).toBe(30);
  });

  it.each([
    [400, 'INVALID_INPUT'],
    [401, 'PROVIDER_UNAVAILABLE'],
    [404, 'NOT_FOUND'],
    [413, 'INVALID_INPUT'],
    [500, 'PROVIDER_UNAVAILABLE'],
  ])('maps HTTP %s to %s', async (status, code) => {
    const koios = new FakeKoios().failOn('tip', status);
    const error = await expectChainDataError(koios.client().get('tip'));
    expect(error.code).toBe(code);
  });

  it('retries a 5xx and returns the eventual success', async () => {
    const koios = new FakeKoios()
      .failOn('tip', 503)
      .thenOn('tip', [{ epoch_no: 7 }]);

    const response = await koios.client({ maxRetries: 1 }).get('tip');
    expect(response.rows).toHaveLength(1);
    expect(koios.callsTo('tip')).toHaveLength(2);
  });

  it('does not retry a client error', async () => {
    const koios = new FakeKoios().failOn('tip', 400).thenOn('tip', [{}]);
    await expect(koios.client({ maxRetries: 2 }).get('tip')).rejects.toThrow();
    expect(koios.callsTo('tip')).toHaveLength(1);
  });

  it('normalises /committee_info, which answers with an object not an array', async () => {
    const koios = new FakeKoios();
    const client = new KoiosHttpClient({
      baseUrl: 'https://koios.test/api/v1',
      fetch: async () =>
        new Response(JSON.stringify({ quorum_numerator: 2 }), {
          status: 200,
          headers: { 'content-type': 'application/json' },
        }),
    });
    void koios;
    const response = await client.get<{ quorum_numerator: number }>(
      'committee_info',
    );
    expect(response.rows).toEqual([{ quorum_numerator: 2 }]);
  });

  it('parses content-range headers', () => {
    expect(parseContentRange('0-999/12345')).toBe(12345);
    expect(parseContentRange('0-999/*')).toBeNull();
    expect(parseContentRange(null)).toBeNull();
  });
});
