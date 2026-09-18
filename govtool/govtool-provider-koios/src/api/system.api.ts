import type {
  Envelope,
  NetworkId,
  ProviderCapabilityDocument,
  ProviderHealth,
  SystemApi,
} from '@govtool/data-providers/chain-data';

import { koiosCapabilities } from '../capabilities';
import { envelope } from '../common/meta';
import type { KoiosHttpClient } from '../http/client';
import { mapTip, networkFromMagic } from '../mappers/network.mapper';
import type { GenesisRow, TipRow } from '../rows';

export const PROVIDER_ID = 'koios';

/** Koios is a public API; a tip older than this means the instance is behind. */
export const DEFAULT_STALENESS_THRESHOLD_SECONDS = 300;

export interface KoiosSystemApiOptions {
  stalenessThresholdSeconds?: number;
  /** Skips the `/genesis` read when the deployment's network is already known. */
  network?: NetworkId;
}

export class KoiosSystemApi implements SystemApi {
  private readonly stalenessThresholdSeconds: number;
  private readonly configuredNetwork: NetworkId | undefined;
  private cachedNetwork: NetworkId | undefined;

  constructor(
    private readonly http: KoiosHttpClient,
    options: KoiosSystemApiOptions = {},
  ) {
    this.stalenessThresholdSeconds =
      options.stalenessThresholdSeconds ?? DEFAULT_STALENESS_THRESHOLD_SECONDS;
    this.configuredNetwork = options.network;
  }

  /**
   * The capability declaration in `../capabilities`, bound to the network this
   * client is pointed at. Nothing is computed here: the document is authored,
   * and `/genesis` only decides which network it claims to describe.
   */
  async getCapabilities(): Promise<Envelope<ProviderCapabilityDocument>> {
    return envelope(koiosCapabilities(await this.network()));
  }

  /**
   * Health is "can we read the tip, and how old is it".
   *
   * Unlike db-sync, the tip carries a timestamp, so staleness is measured
   * rather than left undefined — which is what makes the contract's
   * `secondsSinceLastUpdate` and the 300-second threshold meaningful here.
   */
  async getHealth(): Promise<Envelope<ProviderHealth[]>> {
    const health: ProviderHealth = {
      provider: PROVIDER_ID,
      status: 'healthy',
      stalenessThresholdSeconds: this.stalenessThresholdSeconds,
    };

    try {
      const response = await this.http.get<TipRow>('tip');
      const row = response.rows[0];
      if (row === undefined) {
        return envelope([
          { ...health, status: 'degraded', message: 'Koios returned no tip' },
        ]);
      }

      const tip = mapTip(row);
      health.tip = tip;
      if (tip.time !== undefined) {
        health.lastSuccessfulSyncAt = tip.time;
        const lag = Math.max(
          0,
          Math.round((Date.now() - Date.parse(tip.time)) / 1000),
        );
        health.secondsSinceLastUpdate = lag;
        if (lag > this.stalenessThresholdSeconds) {
          health.status = 'degraded';
          health.message = `Koios tip is ${lag}s old`;
        }
      }
      return envelope([health]);
    } catch (error) {
      return envelope([
        { ...health, status: 'unavailable', message: String(error) },
      ]);
    }
  }

  private async network(): Promise<NetworkId> {
    if (this.configuredNetwork !== undefined) return this.configuredNetwork;
    if (this.cachedNetwork !== undefined) return this.cachedNetwork;
    try {
      const response = await this.http.get<GenesisRow>('genesis');
      this.cachedNetwork = networkFromMagic(response.rows[0]?.networkmagic);
    } catch {
      this.cachedNetwork = 'mainnet';
    }
    return this.cachedNetwork;
  }
}
