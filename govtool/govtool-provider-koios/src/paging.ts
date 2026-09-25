import type { Page, PageRequest } from '@govtool/data-providers/chain-data';

import { invalidInput } from './errors';

/** Largest page served. Above it a request is refused, never silently shortened. */
export const MAX_PAGE_SIZE = 1000;

export interface Window {
  limit: number;
  offset: number;
}

/** Validate a 1-based page request (SPEC.md §3.4) into an offset window. */
export function toWindow(q: PageRequest): Window {
  if (typeof q !== 'object' || q === null) throw invalidInput('A page request { page, size } is required');
  const { page, size } = q;
  if (!Number.isInteger(page) || page < 1) throw invalidInput('page must be an integer from 1', { page });
  if (!Number.isInteger(size) || size < 1 || size > MAX_PAGE_SIZE) {
    throw invalidInput(`size must be an integer from 1 to ${MAX_PAGE_SIZE}`, { size });
  }
  return { limit: size, offset: (page - 1) * size };
}

/** A page cut from a complete, already ordered collection; `total` is exact. */
export function slicePage<T>(all: readonly T[], window: Window): Page<T> {
  return { elements: all.slice(window.offset, window.offset + window.limit), total: all.length };
}
