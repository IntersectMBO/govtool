import { Inject, Injectable } from '@nestjs/common';
import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';

import { CacheService } from 'src/cache/cache.service';
import { asHttp } from 'src/common/errors';
import { LegacyNetwork } from 'src/common/legacy-network';
import { CHAIN_DATA } from 'src/providers/providers.module';
import { AccountInfoResponse } from './account.type';

@Injectable()
export class AccountService {
  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    private readonly cacheService: CacheService,
    private readonly network: LegacyNetwork = new LegacyNetwork(chain),
  ) {}

  async getAccountInfo(stakeKey: string): Promise<AccountInfoResponse> {
    return this.cacheService.getOrSet('accountInfo', stakeKey, () =>
      asHttp(async () => {
        const { data } = await this.chain.accounts.get(
          await this.network.stakeAddress(stakeKey),
        );

        return {
          // The legacy response exposed db-sync's internal row id. No provider
          // carries one now, and the legacy shape has no way to say "absent",
          // so this reports null rather than NaN.
          id: null,
          view: data.stakeAddress,
          isRegistered: data.isRegistered,
          // Optional on the contract: derivable from the address form, so a
          // provider that does not have it cheaply omits it.
          isScriptBased: data.isScriptBased ?? false,
        };
      }),
    );
  }
}
