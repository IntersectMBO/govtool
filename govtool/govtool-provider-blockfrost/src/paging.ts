import type { Page, PageRequest } from '@govtool/data-providers/chain-data';

import { invalidInput } from './errors';
import { BF_PAGE, type BlockfrostHttp } from './http';

/** Largest page served. Above it a request is refused, never silently shortened. */
export const MAX_PAGE_SIZE = 1000;

export interface Window {
  limit: number;
  offset: number;
}

/** Validate a 1-based page request (SPEC.md §3.4). */
export function toWindow(q: PageRequest): Window {
  if (typeof q !== 'object' || q === null) throw invalidInput('A page request { page, size } is required');
  const { page, size } = q;
  if (!Number.isInteger(page) || page < 1) throw invalidInput('page must be an integer from 1', { page });
  if (!Number.isInteger(size) || size < 1 || size > MAX_PAGE_SIZE) {
    throw invalidInput(`size must be an integer from 1 to ${MAX_PAGE_SIZE}`, { size });
  }
  return { limit: size, offset: (page - 1) * size };
}

/** A page of an in-memory collection the provider holds in full: `total` is exact. */
export function slicePage<T>(all: readonly T[], window: Window): Page<T> {
  return { elements: all.slice(window.offset, window.offset + window.limit), total: all.length };
}

/**
 * One contract page read straight off a paged Blockfrost collection, which
 * serves at most 100 rows a page and never a total.
 *
 * The Blockfrost pages covering rows [offset, offset + size) are fetched and
 * sliced, so the contract page is exactly `size` rows unless the collection
 * ends inside it — the only short page SPEC.md §3.4 allows when `total` is
 * absent. `total` is reported only when the end was actually seen: the last
 * covering page came back short and was not empty-at-start.
 */
export async function readWindow<T>(
  http: BlockfrostHttp,
  path: string,
  window: Window,
  order?: 'asc' | 'desc',
): Promise<Page<T>> {
  const first = Math.floor(window.offset / BF_PAGE) + 1;
  const last = Math.floor((window.offset + window.limit - 1) / BF_PAGE) + 1;
  const pages = await Promise.all(
    Array.from({ length: last - first + 1 }, (_, i) =>
      http.get<T[]>(path, { count: BF_PAGE, page: first + i, ...(order ? { order } : {}) }),
    ),
  );
  const rows: T[] = [];
  let endSeenAt: number | undefined;
  for (const [i, page] of pages.entries()) {
    rows.push(...page);
    if (page.length < BF_PAGE) {
      endSeenAt = first + i;
      break;
    }
  }
  const start = window.offset - (first - 1) * BF_PAGE;
  const elements = rows.slice(start, start + window.limit);
  // The end is known when a short page was seen and at least one row of the
  // collection precedes or lies in it; an empty first page says only that the
  // collection holds no more than (first - 1) * 100 rows.
  const total =
    endSeenAt !== undefined && (rows.length > 0 || first === 1) ? (first - 1) * BF_PAGE + rows.length : undefined;
  return total === undefined ? { elements } : { elements, total };
}
