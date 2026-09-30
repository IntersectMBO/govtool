/**
 * `ChainDataApiV1` over the public Koios REST API.
 *
 *   const { chainData } = createKoiosProvider({
 *     network: 'mainnet',          // or 'preprod' / 'preview'
 *     token: process.env.KOIOS_TOKEN, // omit for the free public tier
 *   });
 *
 * Against a self-hosted instance, pass `baseUrl` (including `/api/v1`).
 */
import type { ChainDataApiV1, NetworkId } from '@govtool/data-providers/chain-data';

import { createAccountsApi } from './accounts';
import { createCtx } from './context';
import { createGovernanceApi } from './governance';
import { createKoiosHttp } from './http';
import { createNetworkApi } from './network';
import { createSystemApi } from './system';
import { createTransactionsApi } from './transactions';

export interface KoiosProviderOptions {
  /** The network Koios serves. Decides the default base URL and address prefixes. */
  network: NetworkId;
  /** Koios API token, sent as a Bearer token. Omit for the free public tier. */
  token?: string;
  /** Full base URL including `/api/v1`, for a self-hosted or non-default instance. */
  baseUrl?: string;
  /** Per-request timeout in ms. Default 30 000. */
  timeoutMs?: number;
  /** Retries on 429, 5xx and timeouts. Default 3. */
  maxRetries?: number;
  /** Requests in flight at once. Default 4. */
  maxConcurrency?: number;
  /** Injected in tests; defaults to the global `fetch`. */
  fetch?: typeof globalThis.fetch;
}

export interface KoiosProvider {
  chainData: ChainDataApiV1;
}

export function createKoiosProvider(options: KoiosProviderOptions): KoiosProvider {
  const http = createKoiosHttp(options);
  const ctx = createCtx(http, options.network);
  return {
    chainData: {
      network: createNetworkApi(ctx),
      accounts: createAccountsApi(ctx),
      governance: createGovernanceApi(ctx),
      transactions: createTransactionsApi(ctx),
      system: createSystemApi(ctx),
    },
  };
}

export { capabilities } from './system';
export { KOIOS_BASE_URLS } from './http';
