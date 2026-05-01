import { Injectable, InternalServerErrorException } from '@nestjs/common';

import { assertHexText } from 'src/common/hex';
import { DbService } from 'src/db/db.service';
import { SqlService } from 'src/sql/sq.service';
import { AccountInfoResponse, AccountInfoRow } from './account.type';
import { CacheService } from 'src/cache/cache.service';

@Injectable()
export class AccountService {
  constructor(
    private readonly dbService: DbService,
    private readonly sqlService: SqlService,
    private readonly cacheService: CacheService,
  ) {}

  async getAccountInfo(stakeKey: string): Promise<AccountInfoResponse> {
    return this.cacheService.getOrSet('accountInfo',stakeKey,async()=> {
      assertHexText(stakeKey);

    const sql = this.sqlService.load('get-account-info.sql');
    const result = await this.dbService.query<AccountInfoRow>(sql, [stakeKey]);
    if (result.rows.length !== 1) {
      throw new InternalServerErrorException({
        errorType: 'CriticalError',
        message: 'Could not query the account info.',
      });
    }

    const row = result.rows[0];

    return {
      id: Number(row.id),
      view: row.view,
      isRegistered: row.is_registered,
      isScriptBased: row.is_script_based,
    };
    });
  }
}
