import type { Page, PageRequest } from '@govtool/data-providers/chain-data';

import { invalidInput } from './errors';
import { KOIOS_MAX_PAGE_SIZE } from '../http/client';
import type { KoiosPageOptions, KoiosResponse } from '../http/client';

/**
 * Paging is pushed into PostgREST rather than done in memory: `offset` and
 * `limit` are query parameters Koios understands, so a page costs one request
 * and the whole set is never materialised here.
 *
 * The cursor this provider issues is the next offset as a decimal string —
 * the same convention `@govtool/provider-dbsync` uses, so a consumer that
 * stored a cursor does not have to care which provider it came from.
 *
 * PostgREST caps a response at 1000 rows whatever `limit` says, so "omit
 * `limit` to get everything" — which the contract allows a provider to honour
 * — is not available here. An omitted `limit` fetches one 1000-row page and
 * reports `nextCursor`, and `resolveLimit` is where that is decided.
 */
export function toKoiosPage(q?: PageRequest): {
  offset: number;
  limit: number;
  page: KoiosPageOptions;
} {
  const offset = resolveOffset(q);
  const limit = resolveLimit(q);
  return { offset, limit, page: { offset, limit } };
}

export function resolveLimit(q?: PageRequest): number {
  if (q?.limit === undefined) {
    return KOIOS_MAX_PAGE_SIZE;
  }
  if (!Number.isInteger(q.limit) || q.limit < 0) {
    throw invalidInput('limit must be a non-negative integer', {
      limit: q.limit,
    });
  }
  return Math.min(q.limit, KOIOS_MAX_PAGE_SIZE);
}

export function resolveOffset(q?: PageRequest): number {
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

/**
 * Builds the contract page from what Koios returned.
 *
 * `nextCursor` is set whenever the response filled the requested limit. That
 * can hand back one empty final page rather than ending exactly on the last
 * row — which is the honest answer, because without a total there is no way
 * to tell a full page from the last page.
 */
export function toPage<T>(
  response: KoiosResponse<unknown>,
  elements: T[],
  offset: number,
  limit: number,
): Page<T> {
  const consumed = offset + response.rows.length;
  const hasMore =
    response.total === null
      ? response.rows.length >= limit && limit > 0
      : consumed < response.total;

  const page: Page<T> = {
    elements,
    nextCursor: hasMore ? String(consumed) : null,
  };
  if (response.total !== null) {
    page.total = response.total;
  }
  return page;
}

/**
 * Paging over a list this provider had to assemble in memory — the few reads
 * that join several Koios endpoints and cannot push `offset` down to any one
 * of them.
 */
export function paginateLocally<T>(
  items: readonly T[],
  q?: PageRequest,
): Page<T> {
  const offset = resolveOffset(q);
  const total = items.length;
  if (q?.limit === undefined) {
    return { elements: items.slice(offset), nextCursor: null, total };
  }
  const limit = resolveLimit(q);
  const end = offset + limit;
  return {
    elements: items.slice(offset, end),
    nextCursor: end < total ? String(end) : null,
    total,
  };
}

/**
 * Splits ids into chunks whose JSON body stays under Koios' request-body cap.
 *
 * Koios rejects a POST body over 5,120 bytes with a 413 whose message names
 * the byte count, so the constraint is on bytes and not on the number of ids.
 * Ids differ in length by endpoint — a CIP-129 DRep id is ~62 bytes, a stake
 * address ~64 — so chunks are measured rather than counted. An id that alone
 * exceeds the budget still gets its own chunk, so nothing is silently
 * dropped; Koios will reject that one request rather than the whole read.
 */
export function chunkByBodySize(
  ids: readonly string[],
  maxBytes = 4096,
): string[][] {
  const chunks: string[][] = [];
  let current: string[] = [];
  let size = 0;

  for (const id of ids) {
    // `"<id>",` — the quotes and separator count toward the body.
    const cost = Buffer.byteLength(id, 'utf8') + 3;
    if (current.length > 0 && size + cost > maxBytes) {
      chunks.push(current);
      current = [];
      size = 0;
    }
    current.push(id);
    size += cost;
  }
  if (current.length > 0) chunks.push(current);
  return chunks;
}
