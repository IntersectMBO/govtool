import type {
  CapabilityOverride,
  Envelope,
  NetworkId,
  ProviderCapabilityDocument,
  ProviderHealth,
  SystemApi,
} from '@govtool/data-providers/chain-data';

import { dbSyncCapabilities, PROVIDER_ID } from '../capabilities';
import { envelope } from '../common/meta';
import { runSql } from '../db/run';
import type { Queryable } from '../db/queryable';
import type { NetworkInfoRow } from '../rows';
import { toStrictInteger } from '../common/numbers';

export { PROVIDER_ID };

export interface DbSyncSystemApiOptions {
  /** Above this tip-to-wall-clock gap the provider reports itself unavailable. */
  stalenessThresholdSeconds?: number;
  /**
   * Demotions this INSTANCE needs, from a deployment probe rather than from
   * the code — a db-sync without `utxo_view` is the known case, see
   * `missingUtxoViewOverride`. They are applied to the document on the way out
   * and never merged into the static table, so a fault that clears does not
   * need a release.
   */
  capabilityOverrides?: readonly CapabilityOverride[];
}

export class DbSyncSystemApi implements SystemApi {
  private readonly stalenessThresholdSeconds: number | undefined;
  private readonly capabilityOverrides: readonly CapabilityOverride[];

  constructor(
    private readonly db: Queryable,
    options: DbSyncSystemApiOptions = {},
  ) {
    this.stalenessThresholdSeconds = options.stalenessThresholdSeconds;
    this.capabilityOverrides = options.capabilityOverrides ?? [];
  }

  /**
   * The declaration in `../capabilities`, with this deployment's network and
   * any probed faults. Nothing here inspects the database beyond the network
   * name: what the provider can serve is a property of the frozen SQL, not of
   * the data in it.
   */
  async getCapabilities(): Promise<Envelope<ProviderCapabilityDocument>> {
    const network = await this.networkName();
    return envelope(dbSyncCapabilities(network, this.capabilityOverrides));
  }

  /**
   * Health is "can we read the tip". The statement is the cheapest one here
   * and touches the same tables every other read depends on.
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
      const rows = await runSql<NetworkInfoRow>(
        this.db,
        'get-network-info.sql',
      );
      const row = rows[0];
      if (row?.current_epoch == null || row.current_block == null) {
        return envelope([
          {
            ...health,
            status: 'degraded',
            message: 'db-sync returned no chain tip',
          },
        ]);
      }
      health.tip = {
        epoch: toStrictInteger(row.current_epoch),
        block: toStrictInteger(row.current_block),
      };
      return envelope([health]);
    } catch (error) {
      return envelope([
        { ...health, status: 'unavailable', message: String(error) },
      ]);
    }
  }

  private async networkName(): Promise<NetworkId> {
    try {
      const rows = await runSql<NetworkInfoRow>(
        this.db,
        'get-network-info.sql',
      );
      return rows[0]?.network_name ?? 'mainnet';
    } catch {
      return 'mainnet';
    }
  }
}
