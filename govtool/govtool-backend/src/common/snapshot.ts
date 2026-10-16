import type { Page, PageRequest } from '@govtool/data-providers/chain-data';

/** Rows asked for per request. Page 1 is the first page; there is no offset. */
export const SNAPSHOT_PAGE_SIZE = 500;

/**
 * Reads a whole collection, one page at a time.
 *
 * The cache warmer and both list endpoints work from a *snapshot*: the full
 * set is fetched once, then filtered, sorted and paged in memory. That only
 * works if the fetch really is the full set, and a provider is free to cap a
 * page — Koios returns 1,000 of 1,684 DReps, Blockfrost 25 of the directory.
 * Taking the first page as the snapshot would silently show a fraction of the
 * DReps with no error anywhere, which is worse than failing.
 *
 * With `total` present — which the contract strongly recommends and every
 * provider here supplies — the end is known exactly. Without it the only
 * signal is a short page, which is why the contract forbids returning one for
 * any other reason: a provider that caps `size` below what was asked and omits
 * `total` is indistinguishable from one that has finished.
 *
 * `maxPages` is a guard against a provider whose paging never terminates; it
 * is high enough that no real collection reaches it, and hitting it throws
 * rather than returning a partial snapshot as if it were complete.
 */
export async function readAll<T>(
  fetchPage: (q: PageRequest) => Promise<{ data: Page<T> }>,
  options: { maxPages?: number; size?: number; label?: string } = {},
): Promise<T[]> {
  const maxPages = options.maxPages ?? 500;
  const size = options.size ?? SNAPSHOT_PAGE_SIZE;
  const label = options.label ?? 'snapshot';
  const elements: T[] = [];

  for (let page = 1; page <= maxPages; page += 1) {
    const { data } = await fetchPage({ page, size });
    elements.push(...data.elements);

    if (data.elements.length === 0) {
      return elements;
    }

    if (data.total === undefined) {
      // A short page is the end of the collection, and the contract allows no
      // other reading of one.
      if (data.elements.length < size) {
        return elements;
      }
      continue;
    }

    if (elements.length >= data.total) {
      return elements;
    }

    // More rows to come, but the provider served fewer than it was asked for:
    // the next request would start past the rows it withheld, so continuing
    // would skip them silently.
    if (data.elements.length < size) {
      throw new Error(
        `${label}: provider capped a page at ${data.elements.length} of the ` +
          `${size} requested while reporting ${data.total} rows in total`,
      );
    }
  }

  throw new Error(
    `${label}: provider did not finish paging after ${maxPages} pages`,
  );
}
