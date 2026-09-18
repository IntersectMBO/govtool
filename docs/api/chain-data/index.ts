/**
 * Chain Data API v1 — composed contract.
 *
 * Working draft for https://github.com/IntersectMBO/govtool/issues/4221.
 *
 * Scope: everything derivable from the ledger, served through a swappable
 * provider (db-sync #4222, Koios #4223, Blockfrost #4234, Kupo #4235).
 * Off-chain metadata (../metadata) and author-side pinning (../pinning) are
 * separate components — see ../README.md for the boundaries.
 *
 * Namespaces mirror the routes:
 *   /network/*                network
 *   /accounts/*               accounts
 *   /governance/dreps/*       governance.dreps
 *   /governance/pools/*       governance.pools
 *   /governance/proposals/*   governance.proposals
 *   /governance/votes/*       governance.votes
 *   /governance/committee     governance.committee
 *   /transactions/*           transactions
 *   /system/*                 system
 */

import type { AccountsApi } from "./accounts";
import type { NetworkApi } from "./network";
import type { SystemApi } from "./common";
import type { GovernanceApi } from "./governance";
import type { TransactionsApi } from "./transactions";

export * from "./common";
export * from "./refs";
export * from "./network";
export * from "./accounts";
export * from "./governance";
export * from "./transactions";

export interface ChainDataApiV1 {
  network: NetworkApi;
  accounts: AccountsApi;
  governance: GovernanceApi;
  transactions: TransactionsApi;
  system: SystemApi;
}
