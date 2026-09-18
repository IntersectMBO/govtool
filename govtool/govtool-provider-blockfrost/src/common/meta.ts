import type {
  ChainPoint,
  Envelope,
  ResponseMeta,
} from '@govtool/data-providers/chain-data';

const EMPTY_META: ResponseMeta = Object.freeze({});

/**
 * Most reads omit `asOf`: reporting the tip would cost an extra
 * `/blocks/latest` round trip per response, and the contract says to omit
 * rather than spend one. The routes that already fetch the tip pass it in.
 */
export function envelope<T>(data: T, asOf?: ChainPoint): Envelope<T> {
  return { data, meta: asOf === undefined ? EMPTY_META : { asOf } };
}
