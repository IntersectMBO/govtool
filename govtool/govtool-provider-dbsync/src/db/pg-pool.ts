import { Pool, type PoolConfig } from 'pg';

import type { ClosableQueryable } from './queryable';

/**
 * Pool settings GovTool's backend has always run db-sync with. Applied when
 * the caller does not override them, so a new consumer gets the same
 * connection behaviour the legacy backend was tuned for.
 */
export const DEFAULT_POOL_SETTINGS: Pick<
  PoolConfig,
  'max' | 'idleTimeoutMillis'
> = {
  max: 60,
  idleTimeoutMillis: 1000,
};

export function createPgQueryable(config: PoolConfig): ClosableQueryable {
  const pool = new Pool({ ...DEFAULT_POOL_SETTINGS, ...config });

  return {
    // `pg` constrains its row generic to `QueryResultRow`; `Queryable` does
    // not, so the row type is narrowed here rather than leaking that
    // constraint into every caller.
    query: async (sql, params = []) => ({
      rows: (await pool.query(sql, params as unknown[])).rows,
    }),
    queryArrays: async <T extends unknown[]>(
      sql: string,
      params: readonly unknown[] = [],
    ) => ({
      rows: (
        await pool.query<T>({
          text: sql,
          values: params as unknown[],
          rowMode: 'array',
        })
      ).rows as T[],
    }),
    end: () => pool.end(),
  };
}
