import type { Page, PageRequest } from '@govtool/data-providers/chain-data';

import { invalidInput } from './errors';
import { MAX_PAGE_SIZE } from '../http/client';

/**
 * Translation between the contract's cursor paging and Blockfrost's
 * `count`/`page` paging.
 *
 * Blockfrost pages are 1-based and capped at 100. The contract's cursor is
 * opaque, so this provider issues the next Blockfrost page number as the
 * cursor, which keeps a page boundary aligned with a Blockfrost request and
 * avoids over-fetching.
 *
 * `total` is never set: Blockfrost has no count endpoint for any collection,
 * and the contract marks `total` present "only when the provider can count
 * cheaply".
 */
export interface BlockfrostPage {
  count: number;
  page: number;
  order?: 'asc' | 'desc';
}

export function toBlockfrostPage(
  q: PageRequest | undefined,
  defaults: { count?: number; order?: 'asc' | 'desc' } = {},
): BlockfrostPage {
  const count = Math.min(
    q?.limit ?? defaults.count ?? MAX_PAGE_SIZE,
    MAX_PAGE_SIZE,
  );
  if (!Number.isInteger(count) || count <= 0) {
    throw invalidInput('limit must be a positive integer', { limit: q?.limit });
  }

  let page = 1;
  if (q?.cursor !== undefined) {
    const parsed = Number(q.cursor);
    if (!Number.isInteger(parsed) || parsed < 1) {
      throw invalidInput('cursor was not issued by this provider', {
        cursor: q.cursor,
      });
    }
    page = parsed;
  } else if (q?.offset !== undefined) {
    if (!Number.isInteger(q.offset) || q.offset < 0) {
      throw invalidInput('offset must be a non-negative integer', {
        offset: q.offset,
      });
    }
    if (q.offset % count !== 0) {
      // Blockfrost can only seek in whole pages. Silently rounding would
      // return the wrong rows, so this is refused rather than approximated.
      throw invalidInput(
        `offset must be a multiple of limit (${count}) for this provider; use the cursor instead`,
        { offset: q.offset, limit: count },
      );
    }
    page = q.offset / count + 1;
  }

  return { count, page, ...(defaults.order ? { order: defaults.order } : {}) };
}

/** A page of already-mapped elements, with the next Blockfrost page as cursor. */
export function toContractPage<T>(
  elements: T[],
  requested: BlockfrostPage,
): Page<T> {
  return {
    elements,
    // A short page is the last one — Blockfrost gives no total to compare to.
    nextCursor:
      elements.length < requested.count ? null : String(requested.page + 1),
  };
}

/** Pages an array this provider had to materialise itself. */
export function paginateInMemory<T>(
  items: readonly T[],
  q?: PageRequest,
): Page<T> {
  let offset = 0;
  if (q?.cursor !== undefined) {
    const parsed = Number(q.cursor);
    if (!Number.isInteger(parsed) || parsed < 0) {
      throw invalidInput('cursor was not issued by this provider', {
        cursor: q.cursor,
      });
    }
    offset = parsed;
  } else if (q?.offset !== undefined) {
    if (!Number.isInteger(q.offset) || q.offset < 0) {
      throw invalidInput('offset must be a non-negative integer', {
        offset: q.offset,
      });
    }
    offset = q.offset;
  }

  const total = items.length;
  if (q?.limit === undefined) {
    return { elements: [...items.slice(offset)], nextCursor: null, total };
  }
  if (!Number.isInteger(q.limit) || q.limit < 0) {
    throw invalidInput('limit must be a non-negative integer', {
      limit: q.limit,
    });
  }
  const end = offset + q.limit;
  return {
    elements: [...items.slice(offset, end)],
    nextCursor: end < total ? String(end) : null,
    total,
  };
}
