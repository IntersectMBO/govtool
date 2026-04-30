import { Injectable, InternalServerErrorException } from '@nestjs/common';

import { assertHexText } from 'src/common/hex';
import { DbService } from 'src/db/db.service';
import { SqlService } from 'src/sql/sq.service';
import {
  CurrentDelegationRow,
  DelegationResponse,
  VotingPowerRow,
} from './ada-holder.type';

@Injectable()
export class AdaHolderService {
  constructor(
    private readonly dbService: DbService,
    private readonly sqlService: SqlService,
  ) {}

  async getCurrentDelegation(stakeKey: string): Promise<DelegationResponse | null> {
    assertHexText(stakeKey);

    const sql = this.sqlService.load('get-current-delegation.sql');
    const result = await this.dbService.query<CurrentDelegationRow>(sql, [stakeKey]);
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
  }

  async getVotingPower(stakeKey: string): Promise<number> {
    assertHexText(stakeKey);

    const sql = this.sqlService.load('get-stake-key-voting-power.sql');

    try {
      const result = await this.dbService.query<VotingPowerRow>(sql, [stakeKey]);

      if (result.rows.length !== 1) {
        return 0;
      }

      return this.toInteger(result.rows[0].total_balance);
    } catch {
      return 0;
    }
  }

  private toInteger(value: number | string): number {
    const parsed = Number(value);

    if (!Number.isFinite(parsed)) {
      return 0;
    }

    return Math.floor(parsed);
  }
}
