import { Inject, Injectable } from '@nestjs/common';
import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';

import { CacheService } from 'src/cache/cache.service';
import { asHttp } from 'src/common/errors';
import { CHAIN_DATA } from 'src/providers/providers.module';

@Injectable()
export class EpochService {
  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    private readonly cacheService: CacheService,
  ) {}

  /**
   * The legacy endpoint returns db-sync's `epoch_param` row verbatim, so it is
   * the contract's `raw` that goes on the wire, not the typed subset.
   */
  async getCurrentEpochParams(): Promise<unknown> {
    return this.cacheService.getOrSet('currentEpochParams', 'default', () =>
      asHttp(async () => {
        const { data } = await this.chain.network.getProtocolParams();
        return data.raw;
      }),
    );
  }
}
