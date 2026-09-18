import type { Page, PageRequest } from '@govtool/data-providers/chain-data';

import { readAll } from '../src/common/snapshot';

/** A provider that caps pages and reports the cap through `nextCursor`. */
function pagedProvider(total: number, pageSize: number) {
  const calls: PageRequest[] = [];
  const fetchPage = (q: PageRequest): Promise<{ data: Page<number> }> => {
    calls.push(q);
    const offset = q.cursor === undefined ? 0 : Number(q.cursor);
    const elements = Array.from(
      { length: Math.min(pageSize, total - offset) },
      (_, i) => offset + i,
    );
    const end = offset + elements.length;
    return Promise.resolve({
      data: { elements, nextCursor: end < total ? String(end) : null, total },
    });
  };
  return { calls, fetchPage };
}

describe('readAll (snapshot paging)', () => {
  it('follows the cursor to the end rather than taking the first page', async () => {
    // Koios returns 1,000 of 1,684 DReps; Blockfrost 25 of the directory.
    // Taking the first page would silently show a fraction, with no error.
    const { calls, fetchPage } = pagedProvider(1684, 1000);

    const all = await readAll(fetchPage);

    expect(all).toHaveLength(1684);
    expect(calls).toHaveLength(2);
    expect(calls[0]).toEqual({});
    expect(calls[1]).toEqual({ cursor: '1000' });
  });

  it('makes exactly one request when the provider returns everything', async () => {
    // db-sync materialises the whole set, so nothing changes for it.
    const { calls, fetchPage } = pagedProvider(9, 1000);

    await expect(readAll(fetchPage)).resolves.toHaveLength(9);
    expect(calls).toHaveLength(1);
  });

  it('handles an empty collection', async () => {
    const { fetchPage } = pagedProvider(0, 25);
    await expect(readAll(fetchPage)).resolves.toEqual([]);
  });

  it('stops if a provider repeats a cursor, instead of looping forever', async () => {
    let calls = 0;
    const fetchPage = (): Promise<{ data: Page<number> }> => {
      calls += 1;
      return Promise.resolve({
        data: { elements: [1], nextCursor: 'same', total: 99 },
      });
    };
    const all = await readAll(fetchPage);
    // first page has no cursor, second repeats it and is detected
    expect(calls).toBe(2);
    expect(all).toHaveLength(2);
  });

  it('fails loudly rather than returning a partial snapshot as if complete', async () => {
    const fetchPage = (q: PageRequest): Promise<{ data: Page<number> }> =>
      Promise.resolve({
        data: {
          elements: [1],
          // a cursor that always advances: never terminates
          nextCursor: String(Number(q.cursor ?? '0') + 1),
          total: undefined,
        },
      });

    await expect(
      readAll(fetchPage, { maxPages: 5, label: 'drep list snapshot' }),
    ).rejects.toThrow(/did not finish paging after 5 pages/);
  });
});
