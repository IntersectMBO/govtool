import { fromDatabaseError } from '../common/errors';
import type { Queryable } from './queryable';
import { loadSql, type SqlFileName } from './sql-loader';

/** Runs one of the bundled SQL files; any driver failure becomes a `ChainDataError`. */
export async function runSql<T>(
  db: Queryable,
  file: SqlFileName,
  params: readonly unknown[] = [],
): Promise<T[]> {
  try {
    const result = await db.query<T>(loadSql(file), params);
    return result.rows;
  } catch (error) {
    throw fromDatabaseError(error);
  }
}

export async function runSqlArrays<T extends unknown[]>(
  db: Queryable,
  file: SqlFileName,
  params: readonly unknown[] = [],
): Promise<T[]> {
  try {
    const result = await db.queryArrays<T>(loadSql(file), params);
    return result.rows;
  } catch (error) {
    throw fromDatabaseError(error);
  }
}
