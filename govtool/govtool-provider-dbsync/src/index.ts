export { DbSyncChainDataProvider, createDbSyncProvider } from './provider';
export type { DbSyncProviderOptions } from './provider';

export { createPgQueryable, DEFAULT_POOL_SETTINGS } from './db/pg-pool';
export type {
  ClosableQueryable,
  Queryable,
  QueryResultLike,
} from './db/queryable';
export { loadSql, sqlDirectory } from './db/sql-loader';
export type { SqlFileName } from './db/sql-loader';

export {
  DBSYNC_CAPABILITIES_REVIEWED_AT,
  DBSYNC_PROVIDER_VERSION,
  DBSYNC_REFUSALS,
  dbSyncCapabilities,
  missingUtxoViewOverride,
  PROVIDER_ID,
} from './capabilities';

export { DbSyncAccountsApi } from './api/accounts.api';
export { DbSyncNetworkApi } from './api/network.api';
export { DbSyncSurveysApi } from './api/surveys.api';
export { DbSyncSystemApi } from './api/system.api';
export type { DbSyncSystemApiOptions } from './api/system.api';
export { DbSyncTransactionsApi } from './api/transactions.api';
export {
  DbSyncCommitteeApi,
  DbSyncDRepsApi,
  DbSyncGovernanceApi,
  DbSyncMetricsApi,
  DbSyncPoolsApi,
  DbSyncProposalsApi,
  DbSyncVotesApi,
} from './api/governance';

/**
 * The legacy derivations are exported because a consumer reproducing the
 * pre-contract API response needs the same rules — `status` and `kind` are
 * computed, not stored.
 */
export { deriveKind, deriveStatus } from './mappers/drep.mapper';
export { toContractType, toDbSyncType } from './mappers/proposal.mapper';
export { computeMetadataId } from './common/metadata-id';
export {
  encodeCip129DRepId,
  encodeCip129GovActionId,
  formatLegacyGovActionId,
  normalizeDRepId,
  normalizeStakeKey,
  parseGovActionId,
} from './common/ids';
export { assertHexText, isHexText } from './common/hex';
export type * as Rows from './rows';
