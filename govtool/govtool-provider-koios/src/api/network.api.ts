import type {
  BlockNo,
  BlockSummary,
  Envelope,
  EpochNo,
  EpochSummary,
  NetworkApi,
  NetworkInfo,
  ProtocolParams,
  StakeBasis,
  StakeDistribution,
  Treasury,
} from '@govtool/data-providers/chain-data';

import {
  internal,
  invalidInput,
  notFound,
  unsupported,
} from '../common/errors';
import { envelope } from '../common/meta';
import { toLovelace, toStrictInteger } from '../common/numbers';
import {
  KOIOS_ALWAYS_ABSTAIN,
  KOIOS_ALWAYS_NO_CONFIDENCE,
} from '../common/ids';
import type { KoiosHttpClient } from '../http/client';
import {
  mapBlockSummary,
  mapEpochSummary,
  mapProtocolParams,
  mapTip,
  mapTreasury,
  networkFromMagic,
} from '../mappers/network.mapper';
import type {
  BlockRow,
  DRepEpochSummaryRow,
  DRepInfoRow,
  EpochInfoRow,
  EpochParamsRow,
  GenesisRow,
  TipRow,
  TotalsRow,
} from '../rows';

export class KoiosNetworkApi implements NetworkApi {
  constructor(private readonly http: KoiosHttpClient) {}

  /**
   * `/tip` plus `/genesis` for the network magic and `/epoch_info` for the
   * epoch boundaries — three cheap reads the db-sync provider cannot make at
   * all, which is why `networkMagic` and `era` are `unsupported` there and
   * supported here.
   */
  async getNetworkInfo(): Promise<Envelope<NetworkInfo>> {
    const [tip, genesis, epoch] = await Promise.all([
      this.http.get<TipRow>('tip'),
      this.http.get<GenesisRow>('genesis'),
      this.http.get<EpochInfoRow>(
        'epoch_info',
        {},
        { limit: 1, order: 'epoch_no.desc' },
      ),
    ]);

    const tipRow = tip.rows[0];
    if (tipRow === undefined) {
      throw internal('Koios returned no chain tip.');
    }
    const genesisRow = genesis.rows[0];
    const epochRow = epoch.rows[0];
    const point = mapTip(tipRow);

    const info: NetworkInfo = {
      network: networkFromMagic(genesisRow?.networkmagic),
      tip: point,
      epoch: { no: point.epoch },
    };
    if (genesisRow?.networkmagic !== undefined) {
      info.networkMagic = Number(genesisRow.networkmagic);
    }
    if (tipRow.era !== null && tipRow.era !== undefined) {
      info.era = tipRow.era;
    }
    if (epochRow !== undefined) {
      info.epoch = {
        no: toStrictInteger(epochRow.epoch_no),
        startTime: mapEpochSummary(epochRow).startTime,
        endTime: mapEpochSummary(epochRow).endTime,
      };
    }
    return envelope(info);
  }

  /** `/epoch_info` with no `_epoch_no` returns every epoch, newest first. */
  async listEpochs(q?: {
    limit?: number;
    before?: EpochNo;
  }): Promise<Envelope<EpochSummary[]>> {
    const query = q?.before === undefined ? {} : { epoch_no: `lt.${q.before}` };
    const response = await this.http.get<EpochInfoRow>('epoch_info', query, {
      order: 'epoch_no.desc',
      limit: q?.limit,
    });
    return envelope(response.rows.map(mapEpochSummary));
  }

  /**
   * `/epoch_params?_epoch_no=N` — unlike db-sync's frozen SQL, an arbitrary
   * past epoch is a first-class read here, so a two-year-old ParameterChange
   * action can be rendered against the parameters that were actually in force.
   */
  async getProtocolParams(q?: {
    epoch?: EpochNo;
  }): Promise<Envelope<ProtocolParams>> {
    const query = q?.epoch === undefined ? {} : { _epoch_no: q.epoch };
    const response = await this.http.get<EpochParamsRow>(
      'epoch_params',
      query,
      { order: 'epoch_no.desc', limit: 1 },
    );
    const row = response.rows[0];
    if (row === undefined) {
      throw q?.epoch === undefined
        ? internal('Koios returned no protocol parameters.')
        : notFound('No protocol parameters for that epoch', { epoch: q.epoch });
    }
    return envelope(mapProtocolParams(row));
  }

  /**
   * `/blocks` is **already** newest-first, and asking for it explicitly is
   * pathological: `order=block_height.desc` makes Koios sort the whole block
   * table and the request never returns — measured at >70s against mainnet,
   * versus ~1s without it. So the order is only sent when the query is
   * pinned to one height, where it costs nothing.
   *
   * Koios has no "N blocks before height X" filter, so `block` selects a
   * single block and `limit` takes the newest N; the two do not combine.
   */
  async listBlocks(q?: {
    limit?: number;
    block?: BlockNo;
  }): Promise<Envelope<BlockSummary[]>> {
    if (q?.block !== undefined) {
      const response = await this.http.get<BlockRow>(
        'blocks',
        { block_height: `eq.${q.block}` },
        { order: 'block_height.desc', limit: q.limit },
      );
      return envelope(response.rows.map(mapBlockSummary));
    }
    const response = await this.http.get<BlockRow>(
      'blocks',
      {},
      { limit: q?.limit },
    );
    return envelope(response.rows.map(mapBlockSummary));
  }

  /**
   * Assembled from three endpoints, because Koios has no single distribution
   * read:
   *
   * - `totalActiveStake` and `totalStakeControlledBySPOs` from
   *   `/epoch_info.active_stake` — on Cardano the active stake snapshot *is*
   *   the pool-delegated stake, so the two are the same number by definition,
   *   not by approximation;
   * - `totalStakeControlledByDReps` from `/drep_epoch_summary.amount`;
   * - the two predefined options from `/drep_info`, which accepts
   *   `drep_always_abstain` and `drep_always_no_confidence` as ids.
   *
   * `totalLiveStake` has no source at all and stays absent.
   */
  async getStakeDistribution(q?: {
    epoch?: EpochNo;
    basis?: StakeBasis;
  }): Promise<Envelope<StakeDistribution>> {
    if (q?.basis === 'live') {
      throw unsupported(
        'network.getStakeDistribution{basis:live}',
        'Koios reports the epoch-boundary snapshot only',
      );
    }
    const epochQuery = q?.epoch === undefined ? {} : { _epoch_no: q.epoch };

    const [epochInfo, drepSummary, predefined] = await Promise.all([
      this.http.get<EpochInfoRow>('epoch_info', epochQuery, {
        order: 'epoch_no.desc',
        limit: 1,
      }),
      this.http.get<DRepEpochSummaryRow>('drep_epoch_summary', epochQuery, {
        order: 'epoch_no.desc',
        limit: 1,
      }),
      this.http.post<DRepInfoRow>('drep_info', {
        _drep_ids: [KOIOS_ALWAYS_ABSTAIN, KOIOS_ALWAYS_NO_CONFIDENCE],
      }),
    ]);

    const epochRow = epochInfo.rows[0];
    const summaryRow = drepSummary.rows[0];
    if (epochRow === undefined) {
      throw notFound('No epoch information for that epoch', {
        epoch: q?.epoch,
      });
    }

    const byId = new Map(predefined.rows.map((row) => [row.drep_id, row]));
    const distribution: StakeDistribution = {
      epoch: toStrictInteger(epochRow.epoch_no),
      totalStakeControlledByDReps: toLovelace(summaryRow?.amount ?? '0'),
      totalStakeControlledBySPOs: toLovelace(epochRow.active_stake ?? '0'),
      alwaysAbstainVotingPower: toLovelace(
        byId.get(KOIOS_ALWAYS_ABSTAIN)?.amount ?? '0',
      ),
      alwaysNoConfidenceVotingPower: toLovelace(
        byId.get(KOIOS_ALWAYS_NO_CONFIDENCE)?.amount ?? '0',
      ),
    };
    if (epochRow.active_stake !== null) {
      distribution.totalActiveStake = toLovelace(epochRow.active_stake);
    }
    return envelope(distribution);
  }

  /**
   * `/totals`. Two rows are fetched so `delta` can be filled without a second
   * request — the previous epoch is always the adjacent row.
   */
  async getTreasury(q?: { epoch?: EpochNo }): Promise<Envelope<Treasury>> {
    if (q?.epoch !== undefined && !Number.isInteger(q.epoch)) {
      throw invalidInput('epoch must be an integer', { epoch: q.epoch });
    }
    const query = q?.epoch === undefined ? {} : { epoch_no: `lte.${q.epoch}` };
    const response = await this.http.get<TotalsRow>('totals', query, {
      order: 'epoch_no.desc',
      limit: 2,
    });
    const row = response.rows[0];
    if (row === undefined) {
      throw notFound('No treasury totals for that epoch', { epoch: q?.epoch });
    }
    return envelope(mapTreasury(row, response.rows[1]));
  }
}
