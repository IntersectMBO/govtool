import { Injectable, InternalServerErrorException } from "@nestjs/common";

import { DbService } from "src/db/db.service";
import { SqlService } from "src/sql/sq.service";
import { 
    GetNetworkInfoResponse,
    GetNetworkMetricsResponse,
    GetNetworkTotalStakeResponse, 
    NetworkInfo,
    NetworkMetrics,
  NetworkTotalStake,
 } from "./network.type";
import { CacheService } from "src/cache/cache.service";

@Injectable()
export class NetworkService {
    constructor( 
        private readonly dbService: DbService ,
        private readonly sqlService: SqlService,
        private readonly cacheService: CacheService,
    ) {}

    async getNetworkInfo() : Promise<GetNetworkInfoResponse> {
       return this.cacheService.getOrSet('networkInfo','default',async ()=> {
         const sql = this.sqlService.load('get-network-info.sql')
        const result = await this.dbService.query<NetworkInfo>(sql);

        if (result.rows.length != 1){
            throw this.criticalError('Could not query the network info. This should never happen.');
        }
        const row = result.rows[0];
        if (
            row.current_epoch === null ||
            row.current_block === null ||
            row.network_name ===null
        ) {
            throw this.criticalError('Could not query the network info. This should never happen.');
        }
        return {
             currentTime: new Date().toISOString(),
             epochNo: this.toInteger(row.current_epoch),
             blockNo: this.toInteger(row.current_block),
             networkName: row.network_name,
        };
       });
    }

    async getNetworkTotalStake(): Promise<GetNetworkTotalStakeResponse> {
      return this.cacheService.getOrSet('networkTotalStake','default',async () => {
         const sql = this.sqlService.load('get-network-total-stake.sql');
    const result = await this.dbService.query<NetworkTotalStake>(sql);

    if (result.rows.length !== 1) {
      throw this.criticalError('Could not query the network total stake. This should never happen.');
    }

    const row = result.rows[0];
    return {
      totalStakeControlledByDReps: this.toInteger(row.total_stake_controlled_by_active_dreps),
      totalStakeControlledBySPOs: this.toInteger(row.total_stake_controlled_by_spos),
      alwaysAbstainVotingPower: this.toInteger(row.always_abstain_voting_power),
      alwaysNoConfidenceVotingPower: this.toInteger(row.always_no_confidence_voting_power),
    };
      });
  }

  async getNetworkMetrics(): Promise<GetNetworkMetricsResponse> {
    return this.cacheService.getOrSet('networkMetrics', 'default', async()=>{
      const sql = this.sqlService.load('get-network-metrics.sql');
    const result = await this.dbService.query<NetworkMetrics>(sql);

    if (result.rows.length !== 1) {
      throw this.criticalError('Could not query the network metrics. This should never happen.');
    }

    const row = result.rows[0];

    return {
      uniqueDelegators: this.toInteger(row.unique_delegators),
      totalDelegations: this.toInteger(row.total_delegations),
      totalGovernanceActions: this.toInteger(row.total_gov_action_proposals),
      totalDRepVotes: this.toInteger(row.total_drep_votes),
      totalRegisteredDReps: this.toInteger(row.total_registered_dreps),
      totalDRepDistr: this.toInteger(row.total_drep_distr),
      totalActiveDReps: this.toInteger(row.total_active_dreps),
      totalInactiveDReps: this.toInteger(row.total_inactive_dreps),
      totalActiveCIP119CompliantDReps: this.toInteger(
        row.total_active_cip119_compliant_dreps,
      ),
      totalRegisteredDirectVoters: this.toInteger(row.total_registered_direct_voters),
      noOfCommitteeMembers: this.toInteger(row.no_of_committee_members),
      quorumNumerator: this.toInteger(row.quorum_numerator),
      quorumDenominator: this.toInteger(row.quorum_denominator),
    };
    });
  }

    private toInteger(value: number | string ) : number {
        const parsed = Number(value);

        if (!Number.isInteger(parsed)) {
            throw this.criticalError('Unexpected non-integer value returned from database.');
        }
        return parsed;
    }    

    private criticalError(message: string): InternalServerErrorException {
        return new InternalServerErrorException({
        errorType: 'CriticalError',
        message,
        });
    }
}