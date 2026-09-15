import { Injectable } from "@nestjs/common";

import { DbService } from "src/db/db.service";
import { SqlService } from "src/sql/sq.service";
import { CacheService } from "src/cache/cache.service";
type EpochParams = {
    epoch_param: unknown;
};

@Injectable()
export class EpochService {
    constructor(
        private readonly dbService: DbService,
        private readonly sqlService: SqlService,
        private readonly cacheService: CacheService,
    ) {}

    async getCurrentEpochParams(): Promise<unknown | null > {
        return this.cacheService.getOrSet('currentEpochParams','default',async()=>{
             const sql = this.sqlService.load('get-current-epoch-params.sql');
        const result = await this.dbService.query<EpochParams>(sql);

        if (result.rows.length === 0) {
            return null;
        }

        return result.rows[0].epoch_param;
        });
    };
}