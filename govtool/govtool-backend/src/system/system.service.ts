import { Inject, Injectable } from '@nestjs/common';
import type {
  ChainDataApiV1,
  FeatureSet,
  ProviderCapabilityDocument,
} from '@govtool/data-providers/chain-data';

import { CacheService } from 'src/cache/cache.service';
import { asHttp } from 'src/common/errors';
import { CHAIN_DATA } from 'src/providers/providers.module';
import {
  backendFeatures,
  composeBackendCapabilities,
} from 'src/system/capabilities';

/**
 * `/system/capabilities` and `/system/features`.
 *
 * Both are cached the same way every other read is — `getOrSet` with the
 * configured TTL. A capability document is nearly static, but not entirely:
 * a provider reports a deployment fault through `overrides`, and a fault that
 * clears must reach the browser without a release, so this is a cache with a
 * TTL rather than a value computed once at boot.
 */
@Injectable()
export class SystemService {
  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    private readonly cacheService: CacheService,
  ) {}

  /** The provider's declaration with this backend's raises and losses applied. */
  async getCapabilities(): Promise<ProviderCapabilityDocument> {
    return this.cacheService.getOrSet('systemCapabilities', 'default', () =>
      asHttp(async () => {
        const { data } = await this.chain.system.getCapabilities();
        return composeBackendCapabilities(data);
      }),
    );
  }

  /**
   * The derived feature set — what a component reads. Derived from the
   * COMPOSED document, never from the provider's: a feature set cut from the
   * provider's table would hide the DRep sort the backend supplies and offer
   * the "have I voted?" badge the backend drops.
   */
  async getFeatures(): Promise<FeatureSet> {
    return this.cacheService.getOrSet('systemFeatures', 'default', () =>
      asHttp(async () => backendFeatures(await this.getCapabilities())),
    );
  }
}
