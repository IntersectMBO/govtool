import type { Page, PageRequest } from '@govtool/data-providers/chain-data';

/**
 * Reads a whole collection, following the provider's cursor.
 *
 * The cache warmer and both list endpoints work from a *snapshot*: the full
 * set is fetched once, then filtered, sorted and paged in memory. That only
 * works if the fetch really is the full set.
 *
 * The contract says an omitted `limit` means "everything", but a provider is
 * free to cap it and say so through `nextCursor` — Koios returns 1,000 of
 * 1,684 DReps, Blockfrost 25 of the directory. Taking the first page as the
 * snapshot would silently show a fraction of the DReps with no error
 * anywhere, which is worse than failing. So the cursor is followed until the
 * provider says there is no more.
 *
 * `maxPages` is a guard against a provider whose cursor never terminates; it
 * is high enough that no real collection reaches it, and hitting it throws
 * rather than returning a partial snapshot as if it were complete.
 */
export async function readAll<T>(
  fetchPage: (q: PageRequest) => Promise<{ data: Page<T> }>,
  options: { maxPages?: number; label?: string } = {},
): Promise<T[]> {
  const maxPages = options.maxPages ?? 500;
  const elements: T[] = [];
  let cursor: string | undefined;

  for (let page = 0; page < maxPages; page += 1) {
    const { data } = await fetchPage(cursor === undefined ? {} : { cursor });
    elements.push(...data.elements);

    if (data.nextCursor === null || data.nextCursor === undefined) {
      return elements;
    }
    // A provider that repeats a cursor would loop forever.
    if (data.nextCursor === cursor) {
      return elements;
    }
    cursor = data.nextCursor;
  }

  throw new Error(
    `${options.label ?? 'snapshot'}: provider did not finish paging after ${maxPages} pages`,
  );
}
