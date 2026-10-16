import type { Envelope, NetworkId, Page, PagedEnvelope } from '@govtool/data-providers/chain-data';

import type { BlockfrostHttp } from './http';

export const PROVIDER_ID = 'blockfrost';

/**
 * Reuse within ONE method call. SPEC.md §3.6 puts caching in the consumer, so
 * nothing outlives the call that fetched it; a session only stops one call
 * from reading the same resource twice (the epoch clock, a transaction shared
 * by several proposals, the DRep directory an aggregate and a listing both
 * need).
 */
export class Session {
  private readonly memo = new Map<string, Promise<unknown>>();

  constructor(readonly http: BlockfrostHttp) {}

  once<T>(key: string, load: () => Promise<T>): Promise<T> {
    let hit = this.memo.get(key) as Promise<T> | undefined;
    if (!hit) {
      hit = load();
      this.memo.set(key, hit);
      // A failed load is not remembered, so a retry within the call re-fetches.
      hit.catch(() => this.memo.delete(key));
    }
    return hit;
  }
}

/** Everything an area module needs: the HTTP client, the network, and envelopes. */
export interface Ctx {
  http: BlockfrostHttp;
  network: NetworkId;
  session(): Session;
  envelope<T>(data: T): Envelope<T>;
  paged<T>(page: Page<T>): PagedEnvelope<T>;
}

export function createCtx(http: BlockfrostHttp, network: NetworkId): Ctx {
  const meta = { provider: PROVIDER_ID, network };
  return {
    http,
    network,
    session: () => new Session(http),
    envelope: <T>(data: T) => ({ data, meta: { ...meta } }),
    paged: <T>(page: Page<T>) => ({ data: page, meta: { ...meta } }),
  };
}
