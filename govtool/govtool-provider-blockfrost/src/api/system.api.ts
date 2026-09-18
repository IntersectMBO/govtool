import type {
  Envelope,
  NetworkId,
  ProviderCapabilityDocument,
  ProviderHealth,
  SystemApi,
} from '@govtool/data-providers/chain-data';

import { BLOCKFROST_CAPABILITY_DOCUMENT } from '../capabilities';
import { envelope } from '../common/meta';
import type { BlockfrostClient } from '../http/client';
import type { BfBlock, BfGenesis, BfHealth } from '../http/types';
import { toChainPoint } from '../mappers/network.mapper';

export const PROVIDER_ID = 'blockfrost';

const NETWORK_BY_MAGIC: Record<number, NetworkId> = {
  764824073: 'mainnet',
  1: 'preprod',
  2: 'preview',
  4: 'sanchonet',
};

export interface BlockfrostSystemApiOptions {
  stalenessThresholdSeconds?: number;
}

export class BlockfrostSystemApi implements SystemApi {
  private readonly stalenessThresholdSeconds: number | undefined;
  private genesis: BfGenesis | undefined;

  constructor(
    private readonly client: BlockfrostClient,
    options: BlockfrostSystemApiOptions = {},
  ) {
    this.stalenessThresholdSeconds = options.stalenessThresholdSeconds;
  }

  /**
   * The declaration in `../capabilities`, with `network` resolved from
   * /genesis: one build talks to whichever network it is pointed at, and the
   * rest of the document is a property of the code, not of the deployment.
   */
  async getCapabilities(): Promise<Envelope<ProviderCapabilityDocument>> {
    let network: NetworkId = 'unknown';
    try {
      this.genesis ??= await this.client.get<BfGenesis>('/genesis');
      network = NETWORK_BY_MAGIC[this.genesis.network_magic] ?? 'unknown';
    } catch {
      // Capability reporting must work even when the backend is down.
    }
    return envelope({ ...BLOCKFROST_CAPABILITY_DOCUMENT, network });
  }

  /**
   * `/health` says whether Blockfrost considers itself healthy, and
   * `/blocks/latest` gives the tip and how far behind wall clock it is.
   */
  async getHealth(): Promise<Envelope<ProviderHealth[]>> {
    const health: ProviderHealth = {
      provider: PROVIDER_ID,
      status: 'healthy',
    };
    if (this.stalenessThresholdSeconds !== undefined) {
      health.stalenessThresholdSeconds = this.stalenessThresholdSeconds;
    }

    try {
      const reported = await this.client.get<BfHealth>('/health');
      if (!reported.is_healthy) {
        return envelope([
          {
            ...health,
            status: 'degraded',
            message: 'Blockfrost reports itself unhealthy',
          },
        ]);
      }
    } catch (error) {
      return envelope([
        { ...health, status: 'unavailable', message: String(error) },
      ]);
    }

    try {
      const tip = await this.client.get<BfBlock>('/blocks/latest');
      const point = toChainPoint(tip);
      health.tip = point;
      if (point.time !== undefined) {
        const lag = Math.max(
          0,
          Math.round((Date.now() - Date.parse(point.time)) / 1000),
        );
        health.secondsSinceLastUpdate = lag;
        health.lastSuccessfulSyncAt = point.time;
        if (
          this.stalenessThresholdSeconds !== undefined &&
          lag > this.stalenessThresholdSeconds
        ) {
          return envelope([
            {
              ...health,
              status: 'degraded',
              message: `tip is ${lag}s behind wall clock`,
            },
          ]);
        }
      }
    } catch (error) {
      return envelope([
        { ...health, status: 'degraded', message: String(error) },
      ]);
    }

    return envelope([health]);
  }
}
