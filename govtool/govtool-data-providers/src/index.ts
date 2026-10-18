/**
 * @govtool/data-providers — the interfaces a governance data provider
 * implements so the GovTool backend can read from it.
 *
 * Types only: no implementation, no transport, no framework, zero runtime
 * dependencies. Read SPEC.md first.
 *
 * Six components — two required, four optional:
 *
 *   chain-data        the ledger                              REQUIRED
 *   metadata          anchored off-chain documents            REQUIRED
 *   index-provider    search over actions, DReps and pools    optional
 *   committee-info    identity behind committee credentials   optional
 *   pinning           the author write path                   optional
 *   tx-monitor        mempool and confirmation depth          optional
 */

export * as chainData from './chain-data';
export * as metadata from './metadata';
export * as pinning from './pinning';
export * as governanceIndex from './index-provider';
export * as committeeInfo from './committee-info';
export * as txMonitor from './tx-monitor';

export type { ChainDataApiV1 } from './chain-data';
export type { MetadataServiceV1 } from './metadata';
export type { PinningServiceV1 } from './pinning';
export type { GovernanceIndexV1 } from './index-provider';
export type { CommitteeInfoProviderV1 } from './committee-info';
export type { TransactionMonitorV1 } from './tx-monitor';

export { ChainDataError } from './chain-data';
export { PinningError } from './pinning';
