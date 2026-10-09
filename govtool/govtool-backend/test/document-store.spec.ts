import type {
  MetadataResult,
  MetadataServiceV1,
} from '@govtool/data-providers/metadata';

import {
  DOCUMENT_FETCH_CONCURRENCY,
  DocumentStore,
  forgetDocumentFailure,
} from 'src/metadata/document-store';

const HASH = 'e'.repeat(64);
const MINUTE = 60_000;

const anchor = (url: string) => ({ url, hash: HASH });

const resolved = (body: Record<string, unknown>): MetadataResult => ({
  ok: true,
  hash: HASH,
  body,
  fetchedAt: '2026-10-09T00:00:00Z',
});

const failed: MetadataResult = {
  ok: false,
  code: 'FETCH_ERROR',
  category: 'NETWORK',
  message: 'getaddrinfo ENOTFOUND x',
  checkedAt: '2026-10-09T00:00:00Z',
};

/** A metadata service whose answer per url the test sets. */
function service(answer: (url: string) => Promise<MetadataResult>) {
  const getMetadata = jest.fn((_hash: string, url?: string) =>
    answer(url ?? ''),
  );
  const metadata = {
    getMetadata,
    getCipMetadata: () => Promise.reject(new Error('unused')),
    refresh: () => Promise.reject(new Error('unused')),
    getReport: () => Promise.resolve(null),
    listReports: () => Promise.resolve([]),
  } as unknown as MetadataServiceV1;
  return { metadata, getMetadata };
}

describe('DocumentStore', () => {
  it('keeps a resolved document for good', async () => {
    const { metadata, getMetadata } = service(() =>
      Promise.resolve(resolved({ body: { title: 't' } })),
    );
    const store = new DocumentStore(metadata);

    await store.fill([anchor('https://x/a')]);
    await store.fill([anchor('https://x/a')]);

    expect(store.document(anchor('https://x/a'))).toEqual({
      body: { title: 't' },
    });
    expect(getMetadata).toHaveBeenCalledTimes(1);
  });

  it(`asks for at most ${DOCUMENT_FETCH_CONCURRENCY} documents at once`, async () => {
    let inFlight = 0;
    let most = 0;
    const { metadata } = service(async () => {
      inFlight += 1;
      most = Math.max(most, inFlight);
      await new Promise((resolve) => setImmediate(resolve));
      inFlight -= 1;
      return resolved({});
    });
    const store = new DocumentStore(metadata);

    await store.fill(
      Array.from({ length: 100 }, (_, i) => anchor(`https://x/${i}`)),
    );

    expect(most).toBe(DOCUMENT_FETCH_CONCURRENCY);
  });

  it('retries a reported failure after 5, 10 and 20 minutes, then keeps it', async () => {
    let now = 0;
    const { metadata, getMetadata } = service(() => Promise.resolve(failed));
    const store = new DocumentStore(metadata, () => now);
    const fill = () => store.fill([anchor('https://x/gone')]);

    await fill();
    expect(store.get(anchor('https://x/gone'))).toEqual({
      status: 'failed',
      failure: { code: 'FETCH_ERROR', message: 'getaddrinfo ENOTFOUND x' },
      final: false,
    });

    for (const wait of [5, 10, 20]) {
      now += (wait - 1) * MINUTE;
      await fill();
      const before = getMetadata.mock.calls.length;
      now += MINUTE;
      await fill();
      expect(getMetadata.mock.calls.length).toBe(before + 1);
    }

    // One fetch and three retries; the failure is final now.
    expect(getMetadata).toHaveBeenCalledTimes(4);
    expect(store.get(anchor('https://x/gone'))).toMatchObject({
      status: 'failed',
      final: true,
    });
    now += 24 * 60 * MINUTE;
    await fill();
    expect(getMetadata).toHaveBeenCalledTimes(4);
  });

  it('does not count an attempt the metadata service gave no answer to', async () => {
    let up = false;
    const { metadata, getMetadata } = service(() =>
      up ? Promise.resolve(failed) : Promise.reject(new Error('unreachable')),
    );
    const store = new DocumentStore(metadata, () => 0);
    const fill = () => store.fill([anchor('https://x/a')]);

    await fill();
    await fill();
    expect(store.get(anchor('https://x/a'))).toEqual({ status: 'pending' });
    expect(store.ready).toBe(false);

    // Asked again on the next fill, without a retry delay.
    up = true;
    await fill();
    expect(getMetadata).toHaveBeenCalledTimes(3);
    expect(store.get(anchor('https://x/a'))).toMatchObject({
      status: 'failed',
      final: false,
    });
    expect(store.ready).toBe(true);
  });

  it('stays ready once every anchor has had an answer', async () => {
    const { metadata } = service((url) =>
      url === 'https://x/new'
        ? Promise.reject(new Error('unreachable'))
        : Promise.resolve(resolved({})),
    );
    const store = new DocumentStore(metadata);

    expect(store.ready).toBe(false);
    await store.fill([anchor('https://x/a')]);
    expect(store.ready).toBe(true);

    await store.fill([anchor('https://x/a'), anchor('https://x/new')]);
    expect(store.ready).toBe(true);
    expect(store.get(anchor('https://x/new'))).toEqual({ status: 'pending' });
  });

  it('is ready at once without a metadata service', () => {
    expect(new DocumentStore(null).ready).toBe(true);
  });

  it('drops an anchor no snapshot names any more', async () => {
    const { metadata } = service(() => Promise.resolve(resolved({})));
    const store = new DocumentStore(metadata);

    await store.fill([anchor('https://x/old')]);
    await store.fill([anchor('https://x/new')]);

    expect(store.get(anchor('https://x/old'))).toBeUndefined();
    expect(store.get(anchor('https://x/new'))).toMatchObject({
      status: 'resolved',
    });
  });

  it('asks again after a retry through the metadata routes resolves it', async () => {
    let fixed = false;
    const { metadata } = service(() =>
      Promise.resolve(fixed ? resolved({ body: {} }) : failed),
    );
    let now = 0;
    const store = new DocumentStore(metadata, () => now);
    const fill = () => store.fill([anchor('https://x/a')]);
    for (let i = 0; i < 4; i += 1) {
      await fill();
      now += 60 * MINUTE;
    }
    expect(store.get(anchor('https://x/a'))).toMatchObject({ final: true });

    fixed = true;
    forgetDocumentFailure(HASH.toUpperCase(), 'https://x/a');
    await fill();

    expect(store.document(anchor('https://x/a'))).toEqual({ body: {} });
  });

  it('does not hold an anchor named later behind a slow fetch', async () => {
    let releaseSlow: () => void = () => undefined;
    const { metadata } = service((url) =>
      url === 'https://x/slow'
        ? new Promise((resolve) => {
            releaseSlow = () => resolve(resolved({}));
          })
        : Promise.resolve(resolved({ body: { givenName: 'New' } })),
    );
    const store = new DocumentStore(metadata);

    const first = store.fill([anchor('https://x/slow')]);
    // The next block names a new DRep while the slow fetch is still going.
    const second = store.fill([
      anchor('https://x/slow'),
      anchor('https://x/new'),
    ]);
    await new Promise((resolve) => setImmediate(resolve));

    expect(store.document(anchor('https://x/new'))).toEqual({
      body: { givenName: 'New' },
    });
    expect(store.get(anchor('https://x/slow'))).toEqual({ status: 'pending' });
    releaseSlow();
    await Promise.all([first, second]);
  });

  it('shares one limit across fills that overlap', async () => {
    let inFlight = 0;
    let most = 0;
    const { metadata } = service(async () => {
      inFlight += 1;
      most = Math.max(most, inFlight);
      await new Promise((resolve) => setImmediate(resolve));
      inFlight -= 1;
      return resolved({});
    });
    const store = new DocumentStore(metadata);
    const anchors = Array.from({ length: 50 }, (_, i) =>
      anchor(`https://x/${i}`),
    );

    await Promise.all([store.fill(anchors.slice(0, 40)), store.fill(anchors)]);

    expect(most).toBe(DOCUMENT_FETCH_CONCURRENCY);
    expect(anchors.every((a) => store.document(a) !== undefined)).toBe(true);
  });

  it('asks once for an anchor already being fetched', async () => {
    let release: () => void = () => undefined;
    const { metadata, getMetadata } = service(
      () =>
        new Promise((resolve) => {
          release = () => resolve(resolved({}));
        }),
    );
    const store = new DocumentStore(metadata);

    const first = store.fill([anchor('https://x/a')]);
    const second = store.fill([anchor('https://x/a')]);
    await new Promise((resolve) => setImmediate(resolve));
    release();
    await Promise.all([first, second]);

    expect(getMetadata).toHaveBeenCalledTimes(1);
  });
});
