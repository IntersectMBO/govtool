import type {
  Envelope,
  EpochNo,
  GovernanceMetrics,
  MetricsApi,
} from '@govtool/data-providers/chain-data';

import { internal, unsupported } from '../../common/errors';
import { envelope } from '../../common/meta';
import { runSql } from '../../db/run';
import type { Queryable } from '../../db/queryable';
import { mapMetricsRow } from '../../mappers/network.mapper';
import type { NetworkMetricsRow } from '../../rows';

/** Legacy message, byte for byte. */
export const METRICS_ERROR =
  'Could not query the network metrics. This should never happen.';

export class DbSyncMetricsApi implements MetricsApi {
  constructor(private readonly db: Queryable) {}

  /** The statement is anchored to the current epoch and takes no argument. */
  async get(q?: { epoch?: EpochNo }): Promise<Envelope<GovernanceMetrics>> {
    if (q?.epoch !== undefined) {
      throw unsupported('governance.metrics.get{epoch}');
    }
    const rows = await runSql<NetworkMetricsRow>(
      this.db,
      'get-network-metrics.sql',
    );
    const row = rows[0];
    if (rows.length !== 1 || row === undefined) {
      throw internal(METRICS_ERROR);
    }
    return envelope(mapMetricsRow(row));
  }
}
