import type { Page, PageRequest } from '@govtool/data-providers/chain-data';

import { invalidInput } from './errors';

/** Largest page served. Above it a request is refused, never silently shortened. */
export const MAX_PAGE_SIZE = 1000;

export interface Window {
  limit: number;
  offset: number;
}

/** Validate a 1-based page request (SPEC.md §3.4) into SQL LIMIT/OFFSET. */
export function toWindow(q: PageRequest): Window {
  const { page, size } = q;
  if (!Number.isInteger(page) || page < 1) throw invalidInput('page must be an integer from 1', { page });
  if (!Number.isInteger(size) || size < 1 || size > MAX_PAGE_SIZE) {
    throw invalidInput(`size must be an integer from 1 to ${MAX_PAGE_SIZE}`, { size });
  }
  return { limit: size, offset: (page - 1) * size };
}

/**
 * Build a page from rows selected with `count(*) OVER () AS total_count`, so
 * one query yields both the page and the filtered total. An empty page past
 * the end still needs the total, which the caller supplies from a count query
 * when `rows` is empty and `offset > 0`.
 */
export function toPage<T, R extends { total_count?: unknown }>(rows: R[], map: (row: R) => T, totalWhenEmpty?: number): Page<T> {
  const first = rows[0];
  const total = first ? Number(first.total_count) : totalWhenEmpty;
  return total === undefined ? { elements: rows.map(map) } : { elements: rows.map(map), total };
}
