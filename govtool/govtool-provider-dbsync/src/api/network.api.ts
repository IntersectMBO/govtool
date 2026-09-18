import type {
  BlockNo,
  BlockSummary,
  Envelope,
  EpochNo,
  EpochSummary,
  NetworkApi,
  NetworkInfo,
  ProtocolParams,
  StakeDistribution,
  Treasury,
} from '@govtool/data-providers/chain-data';

import { internal, unsupported } from '../common/errors';
import { envelope } from '../common/meta';
import { runSql } from '../db/run';
import type { Queryable } from '../db/queryable';
import {
  mapEpochParams,
  mapNetworkInfoRow,
  mapTotalStakeRow,
} from '../mappers/network.mapper';
import type {
  EpochParamsRow,
  NetworkInfoRow,
  NetworkTotalStakeRow,
} from '../rows';

/** Legacy messages, byte for byte. */
export const TOTAL_STAKE_ERROR =
  'Could not query the network total stake. This should never happen.';

export class DbSyncNetworkApi implements NetworkApi {
  constructor(private readonly db: Queryable) {}

  async getNetworkInfo(): Promise<Envelope<NetworkInfo>> {
    const rows = await runSql<NetworkInfoRow>(this.db, 'get-network-info.sql');
    const row = rows[0];
    if (rows.length !== 1 || row === undefined) {
      throw internal(
        'Could not query the network info. This should never happen.',
      );
    }
    return envelope(mapNetworkInfoRow(row));
  }

  async getStakeDistribution(): Promise<Envelope<StakeDistribution>> {
    const rows = await runSql<NetworkTotalStakeRow>(
      this.db,
      'get-network-total-stake.sql',
    );
    const row = rows[0];
    if (rows.length !== 1 || row === undefined) {
      throw internal(TOTAL_STAKE_ERROR);
    }
    return envelope(mapTotalStakeRow(row));
  }

  /**
   * Only the current epoch's parameters: `get-current-epoch-params.sql` is
   * `ORDER BY epoch_no DESC LIMIT 1` and takes no argument, so a request for
   * a specific epoch is refused rather than silently answered with the
   * current one.
   */
  async getProtocolParams(q?: {
    epoch?: EpochNo;
  }): Promise<Envelope<ProtocolParams>> {
    if (q?.epoch !== undefined) {
      throw unsupported('network.getProtocolParams{epoch}');
    }
    const rows = await runSql<EpochParamsRow>(
      this.db,
      'get-current-epoch-params.sql',
    );
    const raw = rows[0]?.epoch_param;
    if (raw === undefined || raw === null) {
      throw internal('Could not query the current epoch parameters.');
    }
    return envelope(mapEpochParams(raw));
  }

  listEpochs(_q?: {
    limit?: number;
    before?: EpochNo;
  }): Promise<Envelope<EpochSummary[]>> {
    return Promise.reject(unsupported('network.listEpochs'));
  }

  listBlocks(_q?: {
    limit?: number;
    block?: BlockNo;
  }): Promise<Envelope<BlockSummary[]>> {
    return Promise.reject(unsupported('network.listBlocks'));
  }

  getTreasury(_q?: { epoch?: EpochNo }): Promise<Envelope<Treasury>> {
    return Promise.reject(unsupported('network.getTreasury'));
  }
}
