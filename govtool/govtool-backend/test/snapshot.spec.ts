import type { Page, PageRequest } from '@govtool/data-providers/chain-data';

import { readAll, SNAPSHOT_PAGE_SIZE } from '../src/common/snapshot';

/** A provider that honours `size` and reports `total`, as the contract asks. */
function pagedProvider(total: number, options: { total?: boolean } = {}) {
  const calls: PageRequest[] = [];
  const fetchPage = (q: PageRequest): Promise<{ data: Page<number> }> => {
    calls.push(q);
    const offset = (q.page - 1) * q.size;
    const elements = Array.from(
      { length: Math.max(0, Math.min(q.size, total - offset)) },
      (_, i) => offset + i,
    );
    return Promise.resolve({
      data: {
        elements,
        ...(options.total === false ? {} : { total }),
      },
    });
  };
  return { calls, fetchPage };
}

describe('readAll (snapshot paging)', () => {
  it('follows the pages to the end rather than taking the first', async () => {
    const { calls, fetchPage } = pagedProvider(1684);

    const all = await readAll(fetchPage, { size: 1000 });

    expect(all).toHaveLength(1684);
    expect(calls).toEqual([
      { page: 1, size: 1000 },
      { page: 2, size: 1000 },
    ]);
  });

  it('numbers pages from 1, not 0', async () => {
    const { calls, fetchPage } = pagedProvider(9);
    await readAll(fetchPage);
    expect(calls[0].page).toBe(1);
    expect(calls[0].size).toBe(SNAPSHOT_PAGE_SIZE);
  });

  it('makes exactly one request when everything fits on a page', async () => {
    const { calls, fetchPage } = pagedProvider(9);

    await expect(readAll(fetchPage)).resolves.toHaveLength(9);
    expect(calls).toHaveLength(1);
  });

  it('handles an empty collection', async () => {
    const { fetchPage } = pagedProvider(0);
    await expect(readAll(fetchPage)).resolves.toEqual([]);
  });

  it('takes a short page as the end when there is no total to check', async () => {
    const { calls, fetchPage } = pagedProvider(9, { total: false });
    await expect(readAll(fetchPage)).resolves.toHaveLength(9);
    expect(calls).toHaveLength(1);
  });

  it('reads an exact multiple of the page size to the end', async () => {
    // The last page is full, so the end is only visible through `total`.
    const { calls, fetchPage } = pagedProvider(20);
    await expect(readAll(fetchPage, { size: 10 })).resolves.toHaveLength(20);
    expect(calls).toHaveLength(2);
  });

  it('fails loudly rather than returning a partial snapshot as if complete', async () => {
    // Koios caps a page at 1,000 rows and Blockfrost at 25, whatever `size`
    // asked for. Continuing would start the next page past the rows the
    // provider withheld, so the snapshot would silently lose them.
    const fetchPage = (): Promise<{ data: Page<number> }> =>
      Promise.resolve({ data: { elements: [1, 2], total: 99 } });

    await expect(
      readAll(fetchPage, { size: 10, label: 'drep list snapshot' }),
    ).rejects.toThrow(/capped a page at 2 of the 10 requested/);
  });

  it('gives up rather than paging forever', async () => {
    const fetchPage = (q: PageRequest): Promise<{ data: Page<number> }> =>
      Promise.resolve({
        data: {
          elements: Array.from({ length: q.size }, (_, i) => i),
          total: undefined,
        },
      });

    await expect(
      readAll(fetchPage, { maxPages: 5, size: 2, label: 'drep list snapshot' }),
    ).rejects.toThrow(/did not finish paging after 5 pages/);
  });
});
