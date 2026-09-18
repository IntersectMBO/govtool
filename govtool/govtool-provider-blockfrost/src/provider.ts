import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';

import { BlockfrostAccountsApi } from './api/accounts.api';
import { BlockfrostGovernanceApi } from './api/governance';
import { BlockfrostNetworkApi } from './api/network.api';
import {
  BlockfrostSystemApi,
  type BlockfrostSystemApiOptions,
} from './api/system.api';
import { BlockfrostTransactionsApi } from './api/transactions.api';
import { EpochTimeResolver } from './common/epoch-time';
import { BlockfrostClient, type BlockfrostClientOptions } from './http/client';
import type { BfEpoch } from './http/types';

export interface BlockfrostProviderOptions
  extends BlockfrostClientOptions, BlockfrostSystemApiOptions {}

/**
 * `ChainDataApiV1` over a Blockfrost-compatible HTTP API — verified against
 * the self-hosted blockfrost-ryo 3.1.1.
 *
 * It is a read model and nothing else: no caching of query results, no
 * background refresh. The one thing it memoises is epoch start times, which
 * are immutable facts used to fill the other half of an `EpochStamp`.
 *
 * The shape of this provider is dictated by the shape of the API. Blockfrost
 * is a per-entity store: directory endpoints return ids, and each entity
 * costs its own request. So a page of DReps is two requests per element, and
 * anything that needs the whole collection — a sort, an aggregate, a
 * cross-cutting feed — is refused rather than turned into thousands of
 * requests. Every refusal is declared at `system.getCapabilities()` with the
 * reason.
 *
 * The `surveys` namespace is absent: CIP-179 survey definitions live in
 * transaction metadata, and `/txs/{hash}` answers 500 on this deployment.
 */
export class BlockfrostChainDataProvider implements ChainDataApiV1 {
  readonly network: BlockfrostNetworkApi;
  readonly accounts: BlockfrostAccountsApi;
  readonly governance: BlockfrostGovernanceApi;
  readonly transactions: BlockfrostTransactionsApi;
  readonly system: BlockfrostSystemApi;

  constructor(
    client: BlockfrostClient,
    options: BlockfrostSystemApiOptions = {},
  ) {
    const epochs = new EpochTimeResolver(client, async () => {
      const latest = await client.getOrNull<BfEpoch>('/epochs/latest');
      return latest?.epoch;
    });

    this.network = new BlockfrostNetworkApi(client);
    this.accounts = new BlockfrostAccountsApi(client, epochs);
    this.governance = new BlockfrostGovernanceApi(client, epochs);
    this.transactions = new BlockfrostTransactionsApi();
    this.system = new BlockfrostSystemApi(client, options);
  }
}

export function createBlockfrostProvider(
  options: BlockfrostProviderOptions,
): BlockfrostChainDataProvider {
  return new BlockfrostChainDataProvider(
    new BlockfrostClient(options),
    options,
  );
}
