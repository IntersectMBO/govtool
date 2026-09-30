import { Inject, Injectable } from '@nestjs/common';
import type {
  ChainDataApiV1,
  Envelope,
  ProviderCapabilities,
} from '@govtool/data-providers/chain-data';

import { CacheService } from 'src/cache/cache.service';
import { asHttp } from 'src/common/errors';
import { CHAIN_DATA } from 'src/providers/providers.module';
import { backendFeatures } from 'src/system/capabilities';
import type { FeatureSet } from 'src/system/capabilities';

/**
 * `/system/capabilities` and `/system/features`.
 *
 * Both are cached the same way every other read is — `getOrSet` with the
 * configured TTL. A declaration is nearly static but not entirely: a provider
 * reports a deployment fault by layering it on at read time, and a fault that
 * clears must reach the browser without a release, so this is a cache with a
 * TTL rather than a value computed once at boot.
 */
@Injectable()
export class SystemService {
  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    private readonly cacheService: CacheService,
  ) {}

  /** The configured provider's own declaration, unmodified. For operators. */
  async getCapabilities(): Promise<ProviderCapabilities> {
    return (await this.readCapabilities()).data;
  }

  /**
   * What a component reads: the provider's declaration with this backend's
   * own raises and losses applied. Never the provider's document as-is — that
   * would hide the DRep sort this backend supplies and offer the "have I
   * voted?" badge it drops.
   */
  async getFeatures(): Promise<FeatureSet> {
    return this.cacheService.getOrSet('systemFeatures', 'default', () =>
      asHttp(async () => {
        const { data, meta } = await this.readCapabilities();
        // The network comes off the envelope: a capability claim is about a
        // deployment, and which chain it follows is part of naming it.
        return backendFeatures(data, meta.network);
      }),
    );
  }

  private readCapabilities(): Promise<Envelope<ProviderCapabilities>> {
    return this.cacheService.getOrSet('systemCapabilities', 'default', () =>
      asHttp(() => this.chain.system.getCapabilities()),
    );
  }
}
