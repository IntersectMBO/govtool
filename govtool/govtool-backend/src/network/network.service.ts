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
          epochNo: data.currentEpoch,
          // Optional on a `ChainPoint`: a source may date the tip by slot
          // alone. The legacy field is a required number.
          blockNo: data.tip.block ?? 0,
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

  /**
   * The thirteen legacy counters, assembled from the resources that own them.
   *
   * There is no metrics resource any more: the committee's size and quorum are
   * committee state, the DRep counts are the DRep directory's, and the stake
   * total is the network's. Five of the thirteen had no owner to move to —
   * they counted rows nothing reads — and those are reported as 0 rather than
   * failing the whole response, which would take the dashboard down for
   * numbers no screen renders.
   */
  async getNetworkMetrics(): Promise<GetNetworkMetricsResponse> {
    return this.cacheService.getOrSet('networkMetrics', 'default', () =>
      asHttp(async () => {
        // Written out rather than destructured: `test/capabilities.spec.ts`
        // greps `this.chain.…` for the call sites behind each declared
        // feature, and a shorthand here would hide one.
        const [counts, actions, committee, stake] = await Promise.all([
          this.chain.governance.dreps.getCounts?.(),
          this.chain.governance.proposals.list({ page: 1, size: 1 }),
          this.chain.governance.committee.getCommittee(),
          this.chain.network.getStakeDistribution(),
        ]);

        return {
          uniqueDelegators: 0,
          totalDelegations: 0,
          totalGovernanceActions: actions.data.total ?? 0,
          totalDRepVotes: 0,
          totalRegisteredDReps: counts?.data.totalRegistered ?? 0,
          totalDRepDistr: this.toInteger(
            stake.data.totalStakeControlledByDReps ?? '0',
          ),
          totalActiveDReps: counts?.data.totalActive ?? 0,
          totalInactiveDReps: counts?.data.totalInactive ?? 0,
          // Was "registered with a valid CIP-119 document". Compliance is a
          // property of the document, which chain data no longer resolves.
          totalActiveCIP119CompliantDReps: 0,
          // Direct voters are not a ledger concept and are gone from the
          // contract; the nearest fact, a DRep registered with no anchor, is
          // `anonymous` and is a different set.
          totalRegisteredDirectVoters: 0,
          noOfCommitteeMembers: committee.data.members.length,
          quorumNumerator: committee.data.quorum.numerator,
          quorumDenominator: committee.data.quorum.denominator,
        };
      }),
    );
  }

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
