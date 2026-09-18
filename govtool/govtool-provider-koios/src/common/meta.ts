import type {
  Envelope,
  ResponseMeta,
} from '@govtool/data-providers/chain-data';

/**
 * This provider does not report `asOf`.
 *
 * Koios can report the tip — `/tip` is its cheapest endpoint — but never
 * alongside another query: it would be a second HTTP round trip on every
 * read, and the contract says to omit `asOf` rather than pay for it.
 * Consumers that need the tip call `system.getHealth()`.
 */
const EMPTY_META: ResponseMeta = Object.freeze({});

export function envelope<T>(data: T): Envelope<T> {
  return { data, meta: EMPTY_META };
}
