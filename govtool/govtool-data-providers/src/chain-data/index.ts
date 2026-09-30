/**
 * Chain Data API v1 — everything derivable from the ledger.
 *
 * See SPEC.md §5. Chain data NEVER resolves a URL: it emits anchors and the
 * metadata service fetches them. The one documented exception is the
 * denormalized action `title` on a DRep vote-listing row.
 */

import type { AccountsApi } from './accounts';
import type { GovernanceApi } from './governance';
import type { NetworkApi } from './network';
import type { SystemApi } from './common';
import type { TransactionsApi } from './transactions';

export * from './common';
export * from './refs';
export * from './capabilities';
export * from './network';
export * from './accounts';
export * from './governance';
export * from './transactions';

export interface ChainDataApiV1 {
  network: NetworkApi;
  accounts: AccountsApi;
  governance: GovernanceApi;
  transactions: TransactionsApi;
  system: SystemApi;
}
