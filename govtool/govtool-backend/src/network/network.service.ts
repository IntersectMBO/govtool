import { HttpException, Inject, Injectable } from '@nestjs/common';
import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';

import { CacheService } from 'src/cache/cache.service';
import { asHttp } from 'src/common/errors';
import { dbInteger, type ApiInteger } from 'src/common/integer';
import { CHAIN_DATA } from 'src/providers/providers.module';
import {
  GetNetworkInfoResponse,
  GetNetworkMetricsResponse,
  GetNetworkTotalStakeResponse,
} from './network.type';

@Injectable()
export class NetworkService {
  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    private readonly cacheService: CacheService,
  ) {}

  async getNetworkInfo(): Promise<GetNetworkInfoResponse> {
    return this.cacheService.getOrSet('networkInfo', 'default', () =>
      asHttp(async () => {
        const { data } = await this.chain.network.getNetworkInfo();
        return {
          // Wall clock, not a chain value — the legacy field is the server's
          // own time and is what the frontend compares epoch boundaries to.
          currentTime: new Date().toISOString(),
          epochNo: data.tip.epoch,
          blockNo: data.tip.block,
          networkName: data.network,
        };
      }),
    );
  }

  async getNetworkTotalStake(): Promise<GetNetworkTotalStakeResponse> {
    return this.cacheService.getOrSet('networkTotalStake', 'default', () =>
      asHttp(async () => {
        const { data } = await this.chain.network.getStakeDistribution();
        return {
          totalStakeControlledByDReps: this.required(
            data.totalStakeControlledByDReps,
            'totalStakeControlledByDReps',
          ),
          totalStakeControlledBySPOs: this.required(
            data.totalStakeControlledBySPOs,
            'totalStakeControlledBySPOs',
          ),
          alwaysAbstainVotingPower: this.required(
            data.alwaysAbstainVotingPower,
            'alwaysAbstainVotingPower',
          ),
          alwaysNoConfidenceVotingPower: this.required(
            data.alwaysNoConfidenceVotingPower,
            'alwaysNoConfidenceVotingPower',
          ),
        };
      }),
    );
  }

  async getNetworkMetrics(): Promise<GetNetworkMetricsResponse> {
    return this.cacheService.getOrSet('networkMetrics', 'default', () =>
      asHttp(async () => {
        const { data } = await this.chain.governance.metrics.get();
        return {
          uniqueDelegators: data.uniqueDelegators,
          totalDelegations: data.totalDelegations,
          totalGovernanceActions: data.totalGovernanceActions,
          totalDRepVotes: data.totalDRepVotes,
          totalRegisteredDReps: data.totalRegisteredDReps,
          totalDRepDistr: this.toInteger(data.totalDRepDistribution ?? '0'),
          totalActiveDReps: data.totalActiveDReps,
          totalInactiveDReps: data.totalInactiveDReps,
          totalActiveCIP119CompliantDReps: data.totalActiveCip119CompliantDReps,
          totalRegisteredDirectVoters: data.totalRegisteredDirectVoters,
          noOfCommitteeMembers: data.committee.size,
          quorumNumerator: data.committee.quorum.numerator,
          quorumDenominator: data.committee.quorum.denominator,
        };
      }),
    );
  }

  /**
   * The legacy rule: a value that is not already an integer is a corrupt
   * read and fails the whole response. Applied here to the lovelace figures
   * the contract hands over as strings.
   */
  /**
   * The governance stake breakdown is optional on the contract — only a
   * provider that can aggregate the whole DRep distribution has it — but the
   * legacy response field is a required number. Rather than reporting a
   * missing total as 0, which would read as "no stake", this fails and names
   * the field, so a provider swap surfaces here instead of in the UI.
   */
  private required(value: string | undefined, field: string): ApiInteger {
    if (value === undefined) {
      throw new HttpException(
        {
          errorType: 'NotImplementedError',
          message: `The configured chain-data provider cannot serve ${field}`,
        },
        501,
      );
    }
    return this.toInteger(value);
  }

  /**
   * Exact, not rounded: total supply is above Number.MAX_SAFE_INTEGER, so a
   * stake total that fits in a double stays a number and anything larger
   * becomes a bigint, which the response interceptor writes to JSON without
   * quoting or rounding it.
   */
  private toInteger(value: string | number): ApiInteger {
    return dbInteger(value);
  }
}
