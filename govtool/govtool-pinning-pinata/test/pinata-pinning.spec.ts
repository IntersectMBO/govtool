import { PinningError } from '@govtool/data-providers/pinning';

import {
  PinataPinningService,
  rawBlockCid,
  SINGLE_BLOCK_MAX_BYTES,
} from '../src';

type FetchCall = { url: string; init: RequestInit | undefined };

/** A fetch double that records each call and answers with a canned response. */
function fakeFetch(answer: () => Promise<Response> | Response): {
  fetch: typeof fetch;
  calls: FetchCall[];
} {
  const calls: FetchCall[] = [];
  const impl: typeof fetch = async (input, init) => {
    calls.push({ url: String(input), init });
    return answer();
  };
  return { fetch: impl, calls };
}

const json = (body: unknown, status = 200) =>
  new Response(JSON.stringify(body), { status });

const bytes = (text: string) => new TextEncoder().encode(text);

function service(
  fetchImpl: typeof fetch,
  extra: Partial<ConstructorParameters<typeof PinataPinningService>[0]> = {},
) {
  return new PinataPinningService({
    jwt: 'test-jwt',
    fetch: fetchImpl,
    ...extra,
  });
}

async function rejection(promise: Promise<unknown>): Promise<PinningError> {
  try {
    await promise;
  } catch (error) {
    expect(PinningError.is(error)).toBe(true);
    return error as PinningError;
  }
  throw new Error('expected a rejection');
}

describe('pinData', () => {
  it('sends the legacy multipart upload and returns the cid', async () => {
    const { fetch, calls } = fakeFetch(() =>
      json({ data: { cid: 'bafkreiexample' } }),
    );
    const cid = await service(fetch).pinData(
      bytes('{"body":{}}'),
      'drep1owner',
    );

    expect(cid).toBe('bafkreiexample');
    expect(calls).toHaveLength(1);
    const [call] = calls;
    expect(call!.url).toBe('https://upload.pinata.cloud/v3/files');
    expect(call!.init?.method).toBe('POST');
    expect((call!.init?.headers as Record<string, string>).Authorization).toBe(
      'Bearer test-jwt',
    );
    const body = call!.init?.body as FormData;
    expect(body.get('network')).toBe('public');
    const file = body.get('file') as File;
    expect(file.name).toBe('data.txt');
    expect(file.type).toBe('text/plain');
    expect(await file.text()).toBe('{"body":{}}');
  });

  it('refuses oversized content before any request', async () => {
    const { fetch, calls } = fakeFetch(() => json({}));
    const error = await rejection(
      service(fetch, { maxBytes: 4 }).pinData(bytes('12345'), 'o'),
    );
    expect(error.reason).toBe('TOO_LARGE');
    expect(calls).toHaveLength(0);
  });

  it.each([
    [500, 'BACKEND_ERROR'],
    [429, 'RATE_LIMITED'],
    [503, 'BACKEND_UNAVAILABLE'],
    [413, 'TOO_LARGE'],
  ])(
    'maps an upstream %i to %s with the legacy message',
    async (status, reason) => {
      const { fetch } = fakeFetch(() => new Response('nope', { status }));
      const error = await rejection(service(fetch).pinData(bytes('x'), 'o'));
      expect(error.reason).toBe(reason);
      expect(error.message).toBe(
        `Pinata API returned error status : ${status}`,
      );
    },
  );

  it('rejects a response without a cid', async () => {
    const { fetch } = fakeFetch(() => json({ data: {} }));
    const error = await rejection(service(fetch).pinData(bytes('x'), 'o'));
    expect(error).toMatchObject({
      reason: 'BACKEND_ERROR',
      message: 'Failed to decode Pinata API response',
    });
  });

  it('reports an unreachable backend and a timeout apart', async () => {
    const down = await rejection(
      service(async () => {
        throw new TypeError('fetch failed');
      }).pinData(bytes('x'), 'o'),
    );
    expect(down.reason).toBe('BACKEND_UNAVAILABLE');

    const slow = await rejection(
      service(async () => {
        throw new DOMException('The operation timed out.', 'TimeoutError');
      }).pinData(bytes('x'), 'o'),
    );
    expect(slow.reason).toBe('BACKEND_TIMEOUT');
  });
});

describe('getDataCid', () => {
  it('computes the raw-block CID locally, sending nothing', async () => {
    const { fetch, calls } = fakeFetch(() => json({}));
    await expect(service(fetch).getDataCid(new Uint8Array())).resolves.toBe(
      'bafkreihdwdcefgh4dqkjv67uzcmw7ojee6xedzdetojuzjevtenxquvyku',
    );
    expect(calls).toHaveLength(0);
  });

  it('agrees with rawBlockCid and is stable', async () => {
    const data = bytes('{"hashAlgorithm":"blake2b-256"}');
    const cid = await service(fakeFetch(() => json({})).fetch).getDataCid(data);
    expect(cid).toBe(rawBlockCid(data));
    expect(cid).toMatch(/^bafkrei[a-z2-7]{52}$/);
  });

  it('refuses content larger than one block', async () => {
    const error = await rejection(
      service(fakeFetch(() => json({})).fetch).getDataCid(
        new Uint8Array(SINGLE_BLOCK_MAX_BYTES + 1),
      ),
    );
    expect(error.reason).toBe('TOO_LARGE');
  });
});

describe('fetch', () => {
  it('reads through the gateway', async () => {
    const { fetch, calls } = fakeFetch(() => new Response('hello'));
    const data = await service(fetch).fetch('bafkreiabc');
    expect(new TextDecoder().decode(data)).toBe('hello');
    expect(calls[0]!.url).toBe('https://gateway.pinata.cloud/ipfs/bafkreiabc');
  });

  it('rejects a gateway error', async () => {
    const { fetch } = fakeFetch(() => new Response('', { status: 404 }));
    const error = await rejection(service(fetch).fetch('bafkreiabc'));
    expect(error.reason).toBe('BACKEND_ERROR');
  });
});

describe('unpin', () => {
  it('says why it cannot unpin by CID', async () => {
    const error = await rejection(
      service(fakeFetch(() => json({})).fetch).unpin('bafkreiabc'),
    );
    expect(error.reason).toBe('BACKEND_ERROR');
    expect(error.message).toContain('file id');
  });
});

describe('getHealth', () => {
  it('is healthy when the JWT is accepted', async () => {
    await expect(
      service(fakeFetch(() => json({})).fetch).getHealth(),
    ).resolves.toEqual({ status: 'healthy' });
  });

  it('is degraded when the JWT is refused', async () => {
    const health = await service(
      fakeFetch(() => new Response('', { status: 401 })).fetch,
    ).getHealth();
    expect(health).toEqual({
      status: 'degraded',
      message: 'Pinata authentication check returned 401',
    });
  });

  it('is unavailable when Pinata cannot be reached', async () => {
    const health = await service(async () => {
      throw new TypeError('fetch failed');
    }).getHealth();
    expect(health.status).toBe('unavailable');
  });
});

describe('construction', () => {
  it('refuses an empty jwt', () => {
    expect(() => new PinataPinningService({ jwt: ' ' })).toThrow(
      'non-empty jwt',
    );
  });
});
