import type {
  Envelope,
  ResponseMeta,
} from '@govtool/data-providers/chain-data';

/**
 * This provider does not report `asOf`: none of the legacy statements return
 * the tip alongside their result, and the contract says to omit it rather
 * than spend a round trip. Consumers that need the tip call `system.getHealth()`.
 */
const EMPTY_META: ResponseMeta = Object.freeze({});

export function envelope<T>(data: T): Envelope<T> {
  return { data, meta: EMPTY_META };
}
