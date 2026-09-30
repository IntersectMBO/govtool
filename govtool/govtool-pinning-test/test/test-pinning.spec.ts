import { PinningError } from '@govtool/data-providers/pinning';

import { rawBlockCid, TestPinningService } from '../src';

type FetchCall = { url: string; init: RequestInit | undefined };

/** A fetch double that records each call and answers with a canned response. */
function fakeFetch(answer: (call: FetchCall) => Promise<Response> | Response): {
  fetch: typeof fetch;
  calls: FetchCall[];
} {
  const calls: FetchCall[] = [];
  const impl: typeof fetch = async (input, init) => {
    const call = { url: String(input), init };
    calls.push(call);
    return answer(call);
  };
  return { fetch: impl, calls };
}

const json = (body: unknown, status = 200) =>
  new Response(JSON.stringify(body), { status });

const bytes = (text: string) => new TextEncoder().encode(text);

const BASE_URL = 'http://test-metadata-api:3000/';

function service(
  fetchImpl: typeof fetch,
  extra: Partial<ConstructorParameters<typeof TestPinningService>[0]> = {},
) {
  return new TestPinningService({
    baseUrl: BASE_URL,
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
  it('posts the exact bytes and returns the verified cid', async () => {
    const data = bytes('{"body":{}}');
    const { fetch, calls } = fakeFetch(() => json({ cid: rawBlockCid(data) }));

    const cid = await service(fetch).pinData(data, 'drep1owner');

    expect(cid).toBe(rawBlockCid(data));
    expect(cid).toMatch(/^bafkrei[a-z2-7]{52}$/);
    expect(calls).toHaveLength(1);
    const [call] = calls;
    expect(call!.url).toBe('http://test-metadata-api:3000/ipfs');
    expect(call!.init?.method).toBe('POST');
    const body = call!.init?.body as Blob;
    expect(new Uint8Array(await body.arrayBuffer())).toEqual(data);
  });

  it('refuses a cid that does not name the bytes sent', async () => {
    const { fetch } = fakeFetch(() => json({ cid: rawBlockCid(bytes('y')) }));
    const error = await rejection(service(fetch).pinData(bytes('x'), 'o'));
    expect(error.reason).toBe('BACKEND_ERROR');
    expect(error.message).toContain('whose CID is');
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
    [413, 'TOO_LARGE'],
    [503, 'BACKEND_UNAVAILABLE'],
  ])('maps a %i to %s', async (status, reason) => {
    const { fetch } = fakeFetch(() => new Response('nope', { status }));
    const error = await rejection(service(fetch).pinData(bytes('x'), 'o'));
    expect(error.reason).toBe(reason);
  });

  it('rejects a response without a cid', async () => {
    const { fetch } = fakeFetch(() => json({}));
    const error = await rejection(service(fetch).pinData(bytes('x'), 'o'));
    expect(error.message).toBe(
      'Failed to decode test pinning service response',
    );
  });

  it('reports an unreachable service and a timeout apart', async () => {
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
    await expect(service(fetch).getDataCid(bytes('hello world'))).resolves.toBe(
      'bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e',
    );
    expect(calls).toHaveLength(0);
  });
});

describe('fetch and unpin', () => {
  it('reads through the service gateway route', async () => {
    const { fetch, calls } = fakeFetch(() => new Response('hello'));
    const data = await service(fetch).fetch('bafkreiabc');
    expect(new TextDecoder().decode(data)).toBe('hello');
    expect(calls[0]!.url).toBe('http://test-metadata-api:3000/ipfs/bafkreiabc');
  });

  it('rejects a missing cid', async () => {
    const { fetch } = fakeFetch(() => new Response('', { status: 404 }));
    const error = await rejection(service(fetch).fetch('bafkreiabc'));
    expect(error.reason).toBe('BACKEND_ERROR');
  });

  it('unpins with DELETE', async () => {
    const { fetch, calls } = fakeFetch(() => json({ success: true }));
    await service(fetch).unpin('bafkreiabc');
    expect(calls[0]!.init?.method).toBe('DELETE');
    expect(calls[0]!.url).toBe('http://test-metadata-api:3000/ipfs/bafkreiabc');
  });
});

describe('getHealth', () => {
  it('is healthy on 200', async () => {
    await expect(
      service(fakeFetch(() => json({ status: 'ok' })).fetch).getHealth(),
    ).resolves.toEqual({ status: 'healthy' });
  });

  it('is degraded on any other status', async () => {
    const health = await service(
      fakeFetch(() => new Response('', { status: 404 })).fetch,
    ).getHealth();
    expect(health.status).toBe('degraded');
  });

  it('is unavailable when the service cannot be reached', async () => {
    const health = await service(async () => {
      throw new TypeError('fetch failed');
    }).getHealth();
    expect(health.status).toBe('unavailable');
  });
});

describe('construction', () => {
  it.each(['', ' ', 'not a url', 'file:///etc/passwd'])(
    'refuses baseUrl %p',
    (baseUrl) => {
      expect(() => new TestPinningService({ baseUrl })).toThrow('baseUrl');
    },
  );
});
