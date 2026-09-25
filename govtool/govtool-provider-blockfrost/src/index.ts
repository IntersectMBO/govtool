/**
 * `ChainDataApiV1` over the hosted Blockfrost API.
 *
 *   const { chainData } = createBlockfrostProvider({
 *     network: 'mainnet',
 *     projectId: process.env.BLOCKFROST_PROJECT_ID,
 *   });
 *
 * The project id is sent as the `project_id` header and nowhere else; no error
 * this provider raises contains it.
 */
import type { ChainDataApiV1, NetworkId } from '@govtool/data-providers/chain-data';

import { createAccountsApi } from './accounts';
import { createCtx } from './context';
import { createGovernanceApi } from './governance';
import { BlockfrostHttp } from './http';
import { createNetworkApi } from './network';
import { createSystemApi } from './system';
import { createTransactionsApi } from './transactions';

export interface BlockfrostProviderOptions {
  /** The network Blockfrost serves. Decides stake address prefixes and the hosted URL. */
  network: NetworkId;
  /** Blockfrost project id. Hosted Blockfrost requires one; a self-hosted blockfrost-ryo usually does not. */
  projectId?: string;
  /** Overrides the hosted URL for `network`. */
  baseUrl?: string;
  /** Injected for tests. Defaults to the global `fetch`. */
  fetch?: typeof fetch;
  /** Per-request timeout, ms. Default 30 000. */
  timeoutMs?: number;
  /** Requests in flight at once. Default 8. */
  maxConcurrency?: number;
  /** Retries of a 429, 5xx or transport failure. Default 3. */
  maxRetries?: number;
  /**
   * Client-side request pacing. Default: hosted Blockfrost's 10 per second
   * with bursts of 500 when `baseUrl` is not given; none for a self-hosted
   * one. `null` disables it.
   */
  rateLimit?: { perSecond: number; burst: number } | null;
  /** Injected for tests: the wait between retries. */
  sleep?: (ms: number) => Promise<void>;
}

export interface BlockfrostProvider {
  chainData: ChainDataApiV1;
}

export function createBlockfrostProvider(options: BlockfrostProviderOptions): BlockfrostProvider {
  const http = new BlockfrostHttp(options);
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
export { HOSTED_URLS } from './http';
