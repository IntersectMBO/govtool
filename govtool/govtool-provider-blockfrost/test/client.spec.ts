import { ChainDataError } from '@govtool/data-providers/chain-data';

import { FakeBlockfrost } from './fake-http';

describe('BlockfrostClient', () => {
  it('sends count/page/order and no credential header when none is configured', async () => {
    const bf = new FakeBlockfrost().on('/governance/dreps', []);
    await bf
      .client()
      .get('/governance/dreps', { count: 25, page: 3, order: 'desc' });

    expect(bf.callsTo('/governance/dreps')[0]!.search).toEqual({
      count: '25',
      page: '3',
      order: 'desc',
    });
  });

  it('turns a 404 into null only when asked', async () => {
    const bf = new FakeBlockfrost().onStatus(
      '/governance/dreps/x/metadata',
      404,
    );
    const client = bf.client();

    await expect(
      client.getOrNull('/governance/dreps/x/metadata'),
    ).resolves.toBeNull();
    await expect(
      client.get('/governance/dreps/x/metadata'),
    ).rejects.toMatchObject({
      code: 'NOT_FOUND',
    });
  });

  it.each([
    [400, 'INVALID_INPUT', false],
    [403, 'INTERNAL', false],
    [429, 'PROVIDER_RATE_LIMITED', true],
    [500, 'PROVIDER_UNAVAILABLE', true],
    [504, 'PROVIDER_TIMEOUT', true],
  ])('maps HTTP %s to %s', async (status, code, retryable) => {
    const bf = new FakeBlockfrost().onStatus('/x', status, { error: 'nope' });
    const error = (await bf
      .client({ maxRetries: 0 })
      .get('/x')
      .then(() => null)
      .catch((e: unknown) => e)) as ChainDataError;

    expect(ChainDataError.is(error)).toBe(true);
    expect(error.code).toBe(code);
    expect(error.retryable).toBe(retryable);
  });

  it('retries a 500 and succeeds when a later attempt does', async () => {
    const bf = new FakeBlockfrost();
    let attempt = 0;
    const client = bf.client({
      fetch: (() => {
        attempt += 1;
        return Promise.resolve(
          attempt < 3
            ? new Response('{}', { status: 500 })
            : new Response(JSON.stringify({ ok: true }), { status: 200 }),
        );
      }) as typeof fetch,
      sleep: () => Promise.resolve(),
    });

    await expect(client.get('/x')).resolves.toEqual({ ok: true });
    expect(attempt).toBe(3);
  });

  it('does not retry a 400', async () => {
    let attempt = 0;
    const client = new FakeBlockfrost().client({
      fetch: (() => {
        attempt += 1;
        return Promise.resolve(new Response('{}', { status: 400 }));
      }) as typeof fetch,
      sleep: () => Promise.resolve(),
    });

    await expect(client.get('/x')).rejects.toMatchObject({
      code: 'INVALID_INPUT',
    });
    expect(attempt).toBe(1);
  });

  it('reports a transport failure as PROVIDER_UNAVAILABLE without leaking it', async () => {
    const bf = new FakeBlockfrost().onThrow(
      '/x',
      new TypeError('fetch failed'),
    );
    const error = (await bf
      .client({ maxRetries: 0 })
      .get('/x')
      .then(() => null)
      .catch((e: unknown) => e)) as ChainDataError;

    expect(error.code).toBe('PROVIDER_UNAVAILABLE');
    expect(JSON.stringify(error.toJSON())).not.toContain('fetch failed');
  });

  it('walks pages until one comes back short', async () => {
    const full = Array.from({ length: 100 }, (_, i) => ({ i }));
    const bf = new FakeBlockfrost().onPages('/votes', [
      full,
      full,
      [{ i: 200 }],
    ]);

    await expect(bf.client().getAll('/votes')).resolves.toHaveLength(201);
    expect(bf.callsTo('/votes').map((c) => c.search.page)).toEqual([
      '1',
      '2',
      '3',
    ]);
  });

  it('sends the credential header when a projectId is configured', async () => {
    const seen: Record<string, string>[] = [];
    const client = new FakeBlockfrost().client({
      projectId: 'secret',
      fetch: ((_u: unknown, init?: RequestInit) => {
        seen.push(init?.headers as Record<string, string>);
        return Promise.resolve(new Response('{}', { status: 200 }));
      }) as typeof fetch,
    });
    await client.get('/x');
    expect(seen[0]).toMatchObject({ project_id: 'secret' });
  });
});
