import type { Page, PageRequest } from '@govtool/data-providers/chain-data';

import { invalidInput } from './errors';

/**
 * In-memory paging over a fully materialised list — which is what every list
 * read here is, since the legacy SQL returns the whole set and filtered it in
 * the application. The cursor this provider issues is the next offset as a
 * decimal string; it is opaque to callers but cheap for us.
 *
 * No `limit` means everything from `offset` on, so a consumer that caches and
 * pages in its own process (the legacy backend does) can take the whole set.
 */
export function paginate<T>(items: readonly T[], q?: PageRequest): Page<T> {
  const offset = resolveOffset(q);
  const total = items.length;

  if (q?.limit === undefined) {
    return { elements: items.slice(offset), nextCursor: null, total };
  }
  if (!Number.isInteger(q.limit) || q.limit < 0) {
    throw invalidInput('limit must be a non-negative integer', {
      limit: q.limit,
    });
  }

  const end = offset + q.limit;
  return {
    elements: items.slice(offset, end),
    nextCursor: end < total ? String(end) : null,
    total,
  };
}

function resolveOffset(q?: PageRequest): number {
  if (q?.cursor !== undefined) {
    const parsed = Number(q.cursor);
    if (!Number.isInteger(parsed) || parsed < 0) {
      throw invalidInput('cursor was not issued by this provider', {
        cursor: q.cursor,
      });
    }
    return parsed;
  }
  if (q?.offset !== undefined) {
    if (!Number.isInteger(q.offset) || q.offset < 0) {
      throw invalidInput('offset must be a non-negative integer', {
        offset: q.offset,
      });
    }
    return q.offset;
  }
  return 0;
}
