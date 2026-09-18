import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';

import { KoiosAccountsApi } from './api/accounts.api';
import { KoiosGovernanceApi } from './api/governance';
import { KoiosNetworkApi } from './api/network.api';
import { KoiosSurveysApi } from './api/surveys.api';
import { KoiosSystemApi } from './api/system.api';
import { KoiosTransactionsApi } from './api/transactions.api';
import { KoiosHttpClient } from './http/client';
import type { KoiosHttpOptions } from './http/client';

export interface KoiosProviderOptions extends KoiosHttpOptions {
  /** Above this tip-to-wall-clock gap `system.getHealth()` reports degraded. */
  stalenessThresholdSeconds?: number;
}

/**
 * `ChainDataApiV1` over the Koios REST API.
 *
 * It is a read model and nothing else: no caching, no request coalescing, no
 * background refresh — the same rule `@govtool/provider-dbsync` follows,
 * because those are per-deployment policy and GovTool's backend already owns
 * them. That matters more here than it does over a local database: every read
 * is an HTTP round trip against a shared public service with a rate limit, so
 * a consumer that does not cache will feel it.
 *
 * Compared with the db-sync provider it covers substantially more of the
 * contract — votes, pools, the committee, proposal tallies, typed action
 * bodies, per-epoch voting power, transaction effects — and loses three
 * things: CIP-179 surveys, aggregate DRep metrics, and any per-vote voting
 * power. Every gap is declared at `system.getCapabilities()`; see
 * `koiosCapabilities()` in [`src/capabilities.ts`](./capabilities.ts).
 */
export class KoiosChainDataProvider implements ChainDataApiV1 {
  readonly network: KoiosNetworkApi;
  readonly accounts: KoiosAccountsApi;
  readonly governance: KoiosGovernanceApi;
  readonly transactions: KoiosTransactionsApi;
  readonly surveys: KoiosSurveysApi;
  readonly system: KoiosSystemApi;

  constructor(
    readonly http: KoiosHttpClient,
    options: { stalenessThresholdSeconds?: number } = {},
  ) {
    this.network = new KoiosNetworkApi(http);
    this.accounts = new KoiosAccountsApi(http);
    this.governance = new KoiosGovernanceApi(http);
    this.transactions = new KoiosTransactionsApi(http);
    this.surveys = new KoiosSurveysApi();
    this.system = new KoiosSystemApi(http, options);
  }
}

/**
 * Builds a provider against a public Koios deployment.
 *
 * ```ts
 * const koios = createKoiosProvider({ network: 'mainnet', token: process.env.KOIOS_TOKEN });
 * ```
 */
export function createKoiosProvider(
  options: KoiosProviderOptions = {},
): KoiosChainDataProvider {
  const { stalenessThresholdSeconds, ...http } = options;
  return new KoiosChainDataProvider(new KoiosHttpClient(http), {
    stalenessThresholdSeconds,
  });
}
