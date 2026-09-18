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

import { unsupported } from '../common/errors';
import { envelope } from '../common/meta';
import { mapWithConcurrency } from '../common/concurrency';
import type { BlockfrostClient } from '../http/client';
import type {
  BfBlock,
  BfEpoch,
  BfEpochParameters,
  BfGenesis,
} from '../http/types';
import {
  mapBlockSummary,
  mapEpochSummary,
  mapNetworkInfo,
  mapProtocolParams,
  mapStakeDistribution,
  toChainPoint,
} from '../mappers/network.mapper';

export class BlockfrostNetworkApi implements NetworkApi {
  /** Genesis is immutable for the life of a network, so it is read once. */
  private genesis: BfGenesis | undefined;

  constructor(private readonly client: BlockfrostClient) {}

  async getNetworkInfo(): Promise<Envelope<NetworkInfo>> {
    const [genesis, tip] = await Promise.all([
      this.getGenesis(),
      this.client.get<BfBlock>('/blocks/latest'),
    ]);
    const epoch = await this.client.getOrNull<BfEpoch>('/epochs/latest');
    const info = mapNetworkInfo({ genesis, tip, epoch });
    return envelope(info, info.tip);
  }

  async getProtocolParams(q?: {
    epoch?: EpochNo;
  }): Promise<Envelope<ProtocolParams>> {
    const path =
      q?.epoch === undefined
        ? '/epochs/latest/parameters'
        : `/epochs/${q.epoch}/parameters`;
    const raw = await this.client.get<BfEpochParameters>(path);
    return envelope(mapProtocolParams(raw));
  }

  /**
   * Walks back from the newest epoch. Blockfrost has no epoch-range endpoint,
   * so each epoch is its own read; `limit` defaults to 10 rather than the
   * contract's "everything", because "everything" here means one request per
   * epoch since genesis.
   */
  async listEpochs(q?: {
    limit?: number;
    before?: EpochNo;
  }): Promise<Envelope<EpochSummary[]>> {
    const latest = await this.client.get<BfEpoch>('/epochs/latest');
    const newest = q?.before === undefined ? latest.epoch : q.before - 1;
    const limit = Math.max(1, Math.min(q?.limit ?? 10, 100));
    const wanted = Array.from({ length: limit }, (_, i) => newest - i).filter(
      (epoch) => epoch >= 0,
    );

    const records = await mapWithConcurrency(wanted, 8, (epoch) =>
      epoch === latest.epoch
        ? Promise.resolve(latest)
        : this.client.getOrNull<BfEpoch>(`/epochs/${epoch}`),
    );
    return envelope(
      records
        .filter((r): r is BfEpoch => r !== null)
        .map((r) => mapEpochSummary(r)),
    );
  }

  async listBlocks(q?: {
    limit?: number;
    block?: BlockNo;
  }): Promise<Envelope<BlockSummary[]>> {
    if (q?.block !== undefined) {
      const block = await this.client.getOrNull<BfBlock>(`/blocks/${q.block}`);
      return envelope(block === null ? [] : [mapBlockSummary(block)]);
    }
    const latest = await this.client.get<BfBlock>('/blocks/latest');
    const limit = Math.max(1, Math.min(q?.limit ?? 1, 100));
    if (limit === 1) return envelope([mapBlockSummary(latest)]);

    const height = toChainPoint(latest).block;
    const wanted = Array.from({ length: limit - 1 }, (_, i) => height - i - 1);
    const older = await mapWithConcurrency(wanted, 8, (h) =>
      this.client.getOrNull<BfBlock>(`/blocks/${h}`),
    );
    return envelope([
      mapBlockSummary(latest),
      ...older
        .filter((b): b is BfBlock => b !== null)
        .map((b) => mapBlockSummary(b)),
    ]);
  }

  /**
   * Only the epoch's `active_stake`. The governance breakdown needs the whole
   * DRep distribution aggregated, which is one request per DRep here; and
   * `/network`, which would carry the supply and stake totals, answers 500 on
   * this deployment. See `mapStakeDistribution`.
   */
  async getStakeDistribution(q?: {
    epoch?: EpochNo;
  }): Promise<Envelope<StakeDistribution>> {
    const path =
      q?.epoch === undefined ? '/epochs/latest' : `/epochs/${q.epoch}`;
    const epoch = await this.client.get<BfEpoch>(path);
    return envelope(mapStakeDistribution(epoch));
  }

  getTreasury(_q?: { epoch?: EpochNo }): Promise<Envelope<Treasury>> {
    return Promise.reject(
      unsupported(
        'network.getTreasury',
        'Blockfrost exposes the treasury only through /network, which this deployment answers with a 500',
      ),
    );
  }

  private async getGenesis(): Promise<BfGenesis> {
    this.genesis ??= await this.client.get<BfGenesis>('/genesis');
    return this.genesis;
  }
}
