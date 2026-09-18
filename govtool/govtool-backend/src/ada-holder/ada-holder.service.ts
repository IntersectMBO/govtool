import { Inject, Injectable } from '@nestjs/common';
import type {
  ChainDataApiV1,
  Delegation,
} from '@govtool/data-providers/chain-data';

import { CacheService } from 'src/cache/cache.service';
import { asHttp } from 'src/common/errors';
import { assertHexText } from 'src/common/hex';
import { dbInteger, type ApiInteger } from 'src/common/integer';
import { CHAIN_DATA } from 'src/providers/providers.module';
import { DelegationResponse } from './ada-holder.type';

/** db-sync's `drep_hash.view` values for the two ledger-defined targets. */
const PREDEFINED_VIEW = {
  alwaysAbstain: 'drep_always_abstain',
  alwaysNoConfidence: 'drep_always_no_confidence',
} as const;

@Injectable()
export class AdaHolderService {
  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    private readonly cacheService: CacheService,
  ) {}

  async getCurrentDelegation(
    stakeKey: string,
  ): Promise<DelegationResponse | null> {
    return this.cacheService.getOrSet(
      'adaHolderCurrentDelegation',
      stakeKey,
      () =>
        asHttp(async () => {
          assertHexText(stakeKey);
          const { data } = await this.chain.accounts.getDelegation(stakeKey);
          return data === null ? null : this.toLegacyDelegation(data);
        }),
    );
  }

  /**
   * Zero on any failure, which is what the legacy service did — including for
   * a database outage. Preserved because the frontend renders this number
   * directly and has no error path for it; the provider itself distinguishes
   * "no rows" from "unavailable" for consumers that want the difference.
   */
  async getVotingPower(stakeKey: string): Promise<ApiInteger> {
    return this.cacheService.getOrSet('adaHolderVotingPower', stakeKey, () =>
      asHttp(async () => {
        assertHexText(stakeKey);
        try {
          const { data } = await this.chain.accounts.getVotingPower(stakeKey);
          if (data === null) {
            return 0;
          }
          const amount = Number(data.amount);
          return Number.isFinite(amount) ? Math.floor(amount) : 0;
        } catch {
          return 0;
        }
      }),
    );
  }

  private toLegacyDelegation(delegation: Delegation): DelegationResponse {
    const { target } = delegation;

    if (target.kind === 'drep') {
      return {
        drepHash: target.drep.hash,
        // The legacy field is db-sync's pre-CIP-129 `view`, not the CIP-129 id.
        drepView: target.drep.cip105Id ?? target.drep.id,
        isDRepScriptBased: target.drep.isScriptBased,
        txHash: delegation.txRef?.txHash ?? '',
      };
    }

    return {
      drepHash: null,
      drepView:
        target.kind === 'predefined' ? PREDEFINED_VIEW[target.option] : '',
      isDRepScriptBased: false,
      txHash: delegation.txRef?.txHash ?? '',
    };
  }

  /** Exposed for the numeric coercion used by the controller tests. */
  protected toInteger(value: number | string): ApiInteger {
    return dbInteger(value);
  }
}
