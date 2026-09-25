import { dbInteger, ApiInteger } from 'src/common/integer';
import {
  Injectable,
  InternalServerErrorException,
  Logger,
} from '@nestjs/common';

import { assertHexText } from 'src/common/hex';
import { DbService } from 'src/db/db.service';
import { SqlService } from 'src/sql/sq.service';
import {
  CurrentDelegationRow,
  DelegationResponse,
  VotingPowerRow,
} from './ada-holder.type';
import { CacheService } from 'src/cache/cache.service';
class VotingPowerUnavailableError extends Error {}

@Injectable()
export class AdaHolderService {
  private readonly logger = new Logger(AdaHolderService.name);

  constructor(
    private readonly dbService: DbService,
    private readonly sqlService: SqlService,
    private readonly cacheService: CacheService,
  ) {}

  async getCurrentDelegation(
    stakeKey: string,
  ): Promise<DelegationResponse | null> {
    return this.cacheService.getOrSet(
      'adaHolderCurrentDelegation',
      stakeKey,
      async () => {
        assertHexText(stakeKey);

        const sql = this.sqlService.load('get-current-delegation.sql');
        const result = await this.dbService.query<CurrentDelegationRow>(sql, [
          stakeKey,
        ]);
        if (result.rows.length === 0) {
          return null;
        }

        if (result.rows.length !== 1) {
          throw new InternalServerErrorException({
            errorType: 'CriticalError',
            message: `multiple delegations for stake key: ${stakeKey}`,
          });
        }

        const row = result.rows[0];

        return {
          drepHash: row.drep_raw,
          drepView: row.drep_view,
          isDRepScriptBased: row.has_script,
          txHash: row.encode,
        };
      },
    );
  }

  async getVotingPower(stakeKey: string): Promise<ApiInteger> {
    assertHexText(stakeKey);

    try {
      return await this.cacheService.getOrSet(
        'adaHolderVotingPower',
        stakeKey,
        async () => {
          const sql = this.sqlService.load('get-stake-key-voting-power.sql');

          let result: { rows: VotingPowerRow[] };
          try {
            result = await this.dbService.query<VotingPowerRow>(sql, [
              stakeKey,
            ]);
          } catch (error) {
            this.logger.error(
              `Couldn't fetch voting power for stake key: ${stakeKey}`,
              error instanceof Error ? error.stack : String(error),
            );
            // Rejecting keeps the fallback 0 out of the cache.
            throw new VotingPowerUnavailableError();
          }

          if (result.rows.length === 0) {
            this.logger.warn(
              `No voting power found for stake key: ${stakeKey}`,
            );
            return 0;
          }
          if (result.rows.length !== 1) {
            this.logger.warn(
              `Unexpected voting power result for stake key: ${stakeKey}`,
            );
            return 0;
          }
          return dbInteger(result.rows[0].total_balance);
        },
      );
    } catch (error) {
      // Same response as the Haskell backend: a failed lookup reports 0.
      if (error instanceof VotingPowerUnavailableError) return 0;
      throw error;
    }
  }
}
