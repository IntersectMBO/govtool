import {
  BadRequestException,
  Inject,
  Injectable,
  InternalServerErrorException,
  Logger,
} from '@nestjs/common';
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

/**
 * A lookup that failed rather than found nothing; never cached. An
 * HttpException, so `asHttp` passes it through unchanged.
 */
class VotingPowerUnavailableError extends InternalServerErrorException {}

@Injectable()
export class AdaHolderService {
  private readonly logger = new Logger(AdaHolderService.name);

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
    try {
      return await this.cacheService.getOrSet(
        'adaHolderVotingPower',
        stakeKey,
        () =>
          asHttp(async () => {
            // A malformed key is still a 400, as it was when the legacy route
            // parsed it as hex. Failing to learn the served network (needed
            // only for a bare key hash) is a failure like any other.
            let stakeAddress: string;
            try {
              stakeAddress = await this.network.stakeAddress(stakeKey);
            } catch (error) {
              if (error instanceof BadRequestException) {
                throw error;
              }
              throw this.unavailable(stakeKey, error);
            }
            let data: { amount: string | number } | null;
            try {
              const accounts = withMethod(
                this.chain.accounts,
                'getVotingPower',
                'accounts.getVotingPower',
              );
              ({ data } = await accounts.getVotingPower(stakeAddress));
            } catch (error) {
              throw this.unavailable(stakeKey, error);
            }
            if (data === null) {
              return 0;
            }
            const amount = Number(data.amount);
            return Number.isFinite(amount) ? Math.floor(amount) : 0;
          }),
      );
    } catch (error) {
      // Rejecting inside the cache keeps the fallback 0 out of it.
      if (error instanceof VotingPowerUnavailableError) return 0;
      throw error;
    }
  }

  private unavailable(
    stakeKey: string,
    error: unknown,
  ): VotingPowerUnavailableError {
    this.logger.error(
      `Couldn't fetch voting power for stake key: ${stakeKey}`,
      error instanceof Error ? error.stack : String(error),
    );
    return new VotingPowerUnavailableError();
  }

  private toLegacyDelegation(delegation: Delegation): DelegationResponse {
    const { target } = delegation;

    if (target.kind === 'drep') {
      return {
        // The legacy renderings: the raw hex hash (compared with the wallet's
        // hex DRep id) and the CIP-105 view (searched in the directory). Both
        // equal the `drepId` / `view` on the matching directory row.
        dRepHash: drepIdToHex(target.drep.id),
        dRepView: drepIdToCip105(target.drep.id),
        isDRepScriptBased: target.drep.isScriptBased ?? false,
        txHash: delegation.txRef?.txHash ?? '',
      };
    }

    return {
      dRepHash: null,
      dRepView: PREDEFINED_VIEW[target.target],
      isDRepScriptBased: false,
      txHash: delegation.txRef?.txHash ?? '',
    };
  }

  /** Exposed for the numeric coercion used by the controller tests. */
  protected toInteger(value: number | string): ApiInteger {
    return dbInteger(value);
  }
}
