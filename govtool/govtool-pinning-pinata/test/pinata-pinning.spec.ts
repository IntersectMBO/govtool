import { PinningError } from '@govtool/data-providers/pinning';

import { blake2b256Hex, PinataPinningService } from '../src';

type FetchCall = { url: string; init: RequestInit | undefined };

/** A fetch double that records the call and answers with a canned response. */
function fakeFetch(
  answer: (() => Promise<Response>) | Response,
  calls: FetchCall[] = [],
): { fetch: typeof fetch; calls: FetchCall[] } {
  const impl: typeof fetch = async (input, init) => {
    calls.push({ url: String(input), init });
    return typeof answer === 'function' ? answer() : answer;
  };
  return { fetch: impl, calls };
}

const NOW = new Date('2026-09-18T12:00:00.000Z');

function service(
  fetchImpl: typeof fetch,
  extra: Partial<ConstructorParameters<typeof PinataPinningService>[0]> = {},
) {
  return new PinataPinningService({
    jwt: 'test-jwt',
    fetch: fetchImpl,
    now: () => NOW,
    ...extra,
  });
}

describe('PinataPinningService.pin', () => {
  it('sends the same multipart request the legacy backend sent, and maps the cid', async () => {
    const { fetch, calls } = fakeFetch(
      new Response(JSON.stringify({ data: { cid: 'bafyexample' } }), {
        status: 200,
      }),
    );
    const content = '{"body":{"givenName":"x"}}';

    const record = await service(fetch).pin({
      content,
      contentType: 'text/plain',
      fileName: 'metadata.jsonld',
    });

    expect(calls).toHaveLength(1);
    const [call] = calls;
    expect(call!.url).toBe('https://upload.pinata.cloud/v3/files');
    expect(call!.init?.method).toBe('POST');
    expect((call!.init?.headers as Record<string, string>).Authorization).toBe(
      'Bearer test-jwt',
    );

    const body = call!.init?.body as FormData;
    expect(body).toBeInstanceOf(FormData);
    expect(body.get('network')).toBe('public');
    const file = body.get('file') as File;
    expect(file.name).toBe('metadata.jsonld');
    expect(file.type).toBe('text/plain');
    expect(await file.text()).toBe(content);

    expect(record).toEqual({
      cid: 'bafyexample',
      url: 'ipfs://bafyexample',
      gatewayUrls: ['https://ipfs.io/ipfs/bafyexample'],
      dataHash: blake2b256Hex(content),
      byteSize: Buffer.byteLength(content, 'utf8'),
      contentType: 'text/plain',
      status: 'pinned',
      pinnedAt: NOW.toISOString(),
      replicas: [
        { backend: 'pinata', status: 'pinned', pinnedAt: NOW.toISOString() },
      ],
    });
  });

  it('defaults the file name the way the legacy upload endpoint did', async () => {
    const { fetch, calls } = fakeFetch(
      new Response(JSON.stringify({ data: { cid: 'c' } }), { status: 200 }),
    );
    await service(fetch).pin({ content: 'x', contentType: 'text/plain' });
    expect((calls[0]!.init?.body as FormData).get('file')).toHaveProperty(
      'name',
      'data.txt',
    );
  });

  it('rejects content over 512 KiB before any request leaves', async () => {
    const { fetch, calls } = fakeFetch(new Response('unused'));
    const tooBig = 'a'.repeat(512 * 1024 + 1);

    await expect(
      service(fetch).pin({ content: tooBig, contentType: 'text/plain' }),
    ).rejects.toMatchObject({
      name: 'PinningError',
      reason: 'TOO_LARGE',
      terminal: true,
    });
    expect(calls).toHaveLength(0);
  });

  it('accepts exactly 512 KiB', async () => {
    const { fetch } = fakeFetch(
      new Response(JSON.stringify({ data: { cid: 'c' } })),
    );
    await expect(
      service(fetch).pin({
        content: 'a'.repeat(512 * 1024),
        contentType: 'text/plain',
      }),
    ).resolves.toMatchObject({ cid: 'c' });
  });

  it('reports a network failure as BACKEND_UNAVAILABLE carrying the error text', async () => {
    const { fetch } = fakeFetch(() =>
      Promise.reject(new TypeError('fetch failed')),
    );

    const error = await service(fetch)
      .pin({ content: 'x', contentType: 'text/plain' })
      .catch((e: unknown) => e);

    expect(PinningError.is(error)).toBe(true);
    expect(error).toMatchObject({
      reason: 'BACKEND_UNAVAILABLE',
      message: 'TypeError: fetch failed',
      terminal: false,
      details: { message: 'TypeError: fetch failed' },
    });
  });

  it('reports a non-2xx as BACKEND_ERROR with status and body', async () => {
    const { fetch } = fakeFetch(
      new Response('{"error":"nope"}', { status: 401 }),
    );

    await expect(
      service(fetch).pin({ content: 'x', contentType: 'text/plain' }),
    ).rejects.toMatchObject({
      reason: 'BACKEND_ERROR',
      message: 'Pinata API returned error status : 401',
      details: { status: 401, body: '{"error":"nope"}' },
    });
  });

  it.each([
    ['not json', 'this is not json'],
    ['json without cid', JSON.stringify({ data: {} })],
    ['empty cid', JSON.stringify({ data: { cid: '' } })],
  ])(
    'reports an undecodable 2xx (%s) as BACKEND_INVALID_RESPONSE',
    async (_label, body) => {
      const { fetch } = fakeFetch(new Response(body, { status: 200 }));

      await expect(
        service(fetch).pin({ content: 'x', contentType: 'text/plain' }),
      ).rejects.toMatchObject({
        reason: 'BACKEND_INVALID_RESPONSE',
        terminal: true,
        details: { status: 200, body },
      });
    },
  );
});

describe('PinataPinningService.prepare', () => {
  it('hashes and sizes without touching the network', async () => {
    const { fetch, calls } = fakeFetch(new Response('unused'));
    const result = await service(fetch).prepare({
      content: 'hello',
      contentType: 'application/json',
    });
    expect(result).toEqual({
      dataHash: blake2b256Hex('hello'),
      byteSize: 5,
      valid: true,
    });
    expect(calls).toHaveLength(0);
  });

  it('lists policy violations instead of throwing', async () => {
    const { fetch } = fakeFetch(new Response('unused'));
    const result = await service(fetch, { maxBytes: 3 }).prepare({
      content: 'hello',
      contentType: 'image/png',
    });
    expect(result.valid).toBe(false);
    expect(result.errors).toHaveLength(2);
  });
});

describe('PinataPinningService health and policy', () => {
  it('is healthy when the auth check passes, and remembers the last successful pin', async () => {
    const { fetch } = fakeFetch(
      new Response(JSON.stringify({ data: { cid: 'c' } }), { status: 200 }),
    );
    const svc = service(fetch);
    await svc.pin({ content: 'x', contentType: 'text/plain' });

    await expect(svc.getHealth()).resolves.toEqual([
      {
        backend: 'pinata',
        status: 'healthy',
        lastSuccessAt: NOW.toISOString(),
      },
    ]);
  });

  it('is degraded on a rejected credential and unavailable on a network error', async () => {
    const degraded = service(
      fakeFetch(new Response('', { status: 401 })).fetch,
    );
    await expect(degraded.getHealth()).resolves.toMatchObject([
      { status: 'degraded' },
    ]);

    const down = service(
      fakeFetch(() => Promise.reject(new Error('down'))).fetch,
    );
    await expect(down.getHealth()).resolves.toMatchObject([
      { status: 'unavailable', message: 'Error: down' },
    ]);
  });

  it('reports the policy it enforces and no limits it does not know', async () => {
    await expect(
      service(fakeFetch(new Response('')).fetch).getPolicy(),
    ).resolves.toEqual({
      maxBytes: 512 * 1024,
      allowedContentTypes: [
        'application/ld+json',
        'application/json',
        'text/plain',
      ],
      retentionDays: null,
      requiresAuth: false,
    });
  });

  it('declares the file-management operations unsupported rather than guessing at them', async () => {
    const svc = service(fakeFetch(new Response('')).fetch);
    for (const call of [
      svc.getPin('c'),
      svc.listPins(),
      svc.repin('c'),
      svc.unpin('c'),
    ]) {
      await expect(call).rejects.toMatchObject({
        reason: 'UNSUPPORTED_OPERATION',
        terminal: true,
      });
    }
  });

  it('refuses to construct without a jwt', () => {
    expect(() => new PinataPinningService({ jwt: '' })).toThrow(/jwt/);
  });
});
