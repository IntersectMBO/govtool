/**
 * @govtool/provider-fixture — a full governance data layer over a frozen slice
 * of mainnet.
 *
 * No network, no database, no credentials. Point the backend at this and the
 * whole stack runs locally and deterministically; the same dataset makes
 * integration and frontend tests reproducible.
 *
 *   import { createFixtureProvider } from '@govtool/provider-fixture';
 *   const govtool = createFixtureProvider();
 *   const { data } = await govtool.chainData.governance.dreps.list({ page: 1, size: 10 });
 *
 * Refresh the dataset with `BLOCKFROST_PROJECT_ID=<key> npm run capture`.
 */

import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';
import type { MetadataServiceV1 } from '@govtool/data-providers/metadata';
import type { PinningServiceV1 } from '@govtool/data-providers/pinning';
import type { GovernanceIndexV1 } from '@govtool/data-providers/index-provider';
import type { CommitteeInfoProviderV1 } from '@govtool/data-providers/committee-info';
import type { TransactionMonitorV1 } from '@govtool/data-providers/tx-monitor';

import { loadFixture } from './data';
import type { FixtureData } from './data';
import { createChainData } from './chain-data';
import { createIndex } from './index-provider';
import {
  createCommitteeInfo,
  createMetadataService,
  createPinningService,
  createTxMonitor,
} from './services';

export interface FixtureProvider {
  chainData: ChainDataApiV1;
  metadata: MetadataServiceV1;
  index: GovernanceIndexV1;
  committeeInfo: CommitteeInfoProviderV1;
  pinning: PinningServiceV1;
  txMonitor: TransactionMonitorV1;
  /** The raw dataset, for a test that wants to assert against it directly. */
  data: FixtureData;
}

export interface FixtureOptions {
  /** Path to a dataset; defaults to the bundled mainnet capture. */
  dataFile?: string;
  /** Milliseconds between simulated confirmations. */
  confirmationStepMs?: number;
}

export function createFixtureProvider(options: FixtureOptions = {}): FixtureProvider {
  const data = loadFixture(options.dataFile);
  return {
    chainData: createChainData(data),
    metadata: createMetadataService(data),
    index: createIndex(data),
    committeeInfo: createCommitteeInfo(),
    pinning: createPinningService(),
    txMonitor: createTxMonitor({ stepMs: options.confirmationStepMs }),
    data,
  };
}

export { loadFixture } from './data';
export type { FixtureData } from './data';
