import { Injectable , OnModuleDestroy } from "@nestjs/common";
import { Pool , QueryResult, QueryResultRow} from 'pg';

import { ConfigService } from "src/config/config.service";

@Injectable()
export class DbService implements OnModuleDestroy {
    private readonly pool: Pool;

    constructor(consfigService: ConfigService) {
        this.pool = new Pool({
            ...consfigService.getDbConnectionConfig(),
            max: 60,
            idleTimeoutMillis: 1000,
        });
    }

    query<T extends QueryResultRow = QueryResultRow >(
        sql: string,
        params: unknown[]=[],
    ): Promise<QueryResult<T>> {
        return this.pool.query<T>(sql, params);
    }

    async onModuleDestroy(): Promise<void> {
        await this.pool.end();
    }
}