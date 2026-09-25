import { BadRequestException, Inject, Injectable } from '@nestjs/common';
import type {
  ChainDataApiV1,
  Delegation,
} from '@govtool/data-providers/chain-data';

import { CacheService } from 'src/cache/cache.service';
import { asHttp, withMethod } from 'src/common/errors';
import { drepIdToCip105, drepIdToHex } from 'src/common/legacy-ids';
import { LegacyNetwork } from 'src/common/legacy-network';
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
    private readonly network: LegacyNetwork = new LegacyNetwork(chain),
  ) {}

  async getCurrentDelegation(
    stakeKey: string,
  ): Promise<DelegationResponse | null> {
    return this.cacheService.getOrSet(
      'adaHolderCurrentDelegation',
      stakeKey,
      () =>
        asHttp(async () => {
          const stakeAddress = await this.network.stakeAddress(stakeKey);
          try {
            const { data } =
              await this.chain.accounts.getDelegation(stakeAddress);
            return data === null ? null : this.toLegacyDelegation(data);
          } catch (error) {
            // The legacy statement answered a stake address it had never
            // seen with no row, so null — which is what a fresh wallet gets.
            if (
              typeof error === 'object' &&
              error !== null &&
              (error as { code?: unknown }).code === 'NOT_FOUND'
            ) {
              return null;
            }
            throw error;
          }
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
        // A malformed key is still a 400, as it was when the legacy route
        // parsed it as hex. Failing to learn the served network (needed only
        // for a bare key hash) is a failure like any other: 0.
        let stakeAddress: string;
        try {
          stakeAddress = await this.network.stakeAddress(stakeKey);
        } catch (error) {
          if (error instanceof BadRequestException) {
            throw error;
          }
          return 0;
        }
        try {
          const accounts = withMethod(
            this.chain.accounts,
            'getVotingPower',
            'accounts.getVotingPower',
          );
          const { data } = await accounts.getVotingPower(stakeAddress);
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
        // The legacy renderings: the raw hex hash (compared with the wallet's
        // hex DRep id) and the CIP-105 view (searched in the directory). Both
        // equal the `drepId` / `view` on the matching directory row.
        drepHash: drepIdToHex(target.drep.id),
        drepView: drepIdToCip105(target.drep.id),
        isDRepScriptBased: target.drep.isScriptBased ?? false,
        txHash: delegation.txRef?.txHash ?? '',
      };
    }

    return {
      drepHash: null,
      drepView: PREDEFINED_VIEW[target.target],
      isDRepScriptBased: false,
      txHash: delegation.txRef?.txHash ?? '',
    };
  }

  /** Exposed for the numeric coercion used by the controller tests. */
  protected toInteger(value: number | string): ApiInteger {
    return dbInteger(value);
  }
}
