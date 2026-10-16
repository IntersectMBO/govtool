import type { Envelope, NetworkId, Page, PagedEnvelope } from '@govtool/data-providers/chain-data';

import { createChain, type Chain } from './chain';
import type { KoiosHttp } from './http';

export const PROVIDER_ID = 'koios';

/** Everything an area module needs: the transport, the network, chain time, envelopes. */
export interface Ctx {
  http: KoiosHttp;
  network: NetworkId;
  chain: Chain;
  envelope<T>(data: T): Envelope<T>;
  paged<T>(page: Page<T>): PagedEnvelope<T>;
}

export function createCtx(http: KoiosHttp, network: NetworkId): Ctx {
  const meta = { provider: PROVIDER_ID, network };
  return {
    http,
    network,
    chain: createChain(http, network),
    envelope: <T>(data: T) => ({ data, meta: { ...meta } }),
    paged: <T>(page: Page<T>) => ({ data: page, meta: { ...meta } }),
  };
}
