import { Inject, Injectable } from '@nestjs/common';
import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';

import { CacheService } from 'src/cache/cache.service';
import { asHttp } from 'src/common/errors';
import { assertHexText } from 'src/common/hex';
import { CHAIN_DATA } from 'src/providers/providers.module';
import { AccountInfoResponse } from './account.type';

@Injectable()
export class AccountService {
  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    private readonly cacheService: CacheService,
  ) {}

  async getAccountInfo(stakeKey: string): Promise<AccountInfoResponse> {
    return this.cacheService.getOrSet('accountInfo', stakeKey, () =>
      asHttp(async () => {
        assertHexText(stakeKey);
        const { data } = await this.chain.accounts.get(stakeKey);

        return {
          // The legacy response exposed db-sync's internal row id. The
          // contract keeps it opaque, so it is narrowed back here.
          id: Number(data.providerId),
          view: data.stakeAddress,
          isRegistered: data.isRegistered,
          isScriptBased: data.isScriptBased,
        };
      }),
    );
  }
}
