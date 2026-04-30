import { Injectable } from "@nestjs/common";

import { DbService } from "src/db/db.service";
import { SqlService } from "src/sql/sq.service";
type EpochParams = {
    epoch_param: unknown;
};

@Injectable()
export class EpochService {
    constructor(
        private readonly dbService: DbService,
        private readonly sqlService: SqlService,
    ) {}

    async getCurrentEpochParams(): Promise<unknown | null > {
        const sql = this.sqlService.load('get-current-epoch-params.sql');
        const result = await this.dbService.query<EpochParams>(sql);

        if (result.rows.length === 0) {
            return null;
        }

        return result.rows[0].epoch_param;
    }
}