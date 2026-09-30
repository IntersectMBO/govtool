import type { Envelope, NetworkId, Page, PagedEnvelope } from '@govtool/data-providers/chain-data';

import type { Db } from './db';

export const PROVIDER_ID = 'dbsync';

/** Everything an area module needs: the database, the network, and envelopes. */
export interface Ctx {
  db: Db;
  network: NetworkId;
  envelope<T>(data: T): Envelope<T>;
  paged<T>(page: Page<T>): PagedEnvelope<T>;
}

export function createCtx(db: Db, network: NetworkId): Ctx {
  const meta = { provider: PROVIDER_ID, network };
  return {
    db,
    network,
    envelope: <T>(data: T) => ({ data, meta: { ...meta } }),
    paged: <T>(page: Page<T>) => ({ data: page, meta: { ...meta } }),
  };
}
