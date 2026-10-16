import type { Envelope, Page, PageRequest } from '@govtool/data-providers/chain-data';
import { ChainDataError } from '@govtool/data-providers/chain-data';

export const PROVIDER = 'fixture';

export function envelope<T>(data: T, network: string): Envelope<T> {
  return { data, meta: { provider: PROVIDER, network } };
}

/**
 * `page` is 1-based and `total` is the whole filtered set, not this page.
 *
 * The fixture always supplies `total` — it holds everything in memory, so
 * there is no excuse not to, and a consumer can render a numbered paginator.
 */
export function paginate<T>(rows: readonly T[], q: PageRequest): Page<T> {
  const size = Math.max(1, q.size);
  const page = Math.max(1, q.page);
  const start = (page - 1) * size;
  return { elements: rows.slice(start, start + size), total: rows.length };
}

export function pagedEnvelope<T>(
  rows: readonly T[],
  q: PageRequest,
  network: string,
): Envelope<Page<T>> {
  return envelope(paginate(rows, q), network);
}

export function notFound(what: string, id: string): ChainDataError {
  return new ChainDataError('NOT_FOUND', `${what} not found: ${id}`, { details: { id } });
}

export function invalid(message: string): ChainDataError {
  return new ChainDataError('INVALID_INPUT', message);
}
