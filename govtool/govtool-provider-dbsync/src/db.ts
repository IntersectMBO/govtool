import { ChainDataError } from '@govtool/data-providers/chain-data';
import { Pool, types, type PoolConfig } from 'pg';

/**
 * The one thing this provider needs from a database: run parameterised SQL and
 * get rows back. An interface, so tests drive the provider with a fake and a
 * consumer that owns a pool can hand it in.
 */
export interface Db {
  query<T = Record<string, unknown>>(sql: string, params?: readonly unknown[]): Promise<T[]>;
}

export interface ClosableDb extends Db {
  end(): Promise<void>;
}

/** Postgres `query_canceled`, which a statement timeout raises. */
const QUERY_CANCELED = '57014';

/**
 * Every driver failure becomes a ChainDataError. The message stays generic so a
 * connection string or table name never reaches a response; the original goes
 * on `cause` for logs.
 */
export function fromDatabaseError(error: unknown): ChainDataError {
  if (ChainDataError.is(error)) return error;
  const code = (error as { code?: unknown } | null)?.code;
  if (code === QUERY_CANCELED) {
    return new ChainDataError('PROVIDER_TIMEOUT', 'db-sync query timed out', {
      retryable: true,
      cause: error,
    });
  }
  return new ChainDataError('PROVIDER_UNAVAILABLE', 'db-sync query failed', {
    retryable: true,
    cause: error,
  });
}

/**
 * Connection settings. Declared here rather than as `pg`'s `PoolConfig`, so the
 * published types do not depend on `@types/pg`, which a consumer that prunes
 * dev dependencies does not have.
 */
export interface PgDbOptions {
  host: string;
  port?: number;
  database: string;
  user: string;
  password?: string;
  /** Passed to `pg` as is: `true`, or TLS options. */
  ssl?: boolean | Record<string, unknown>;
  /** Largest number of pooled connections. Default 20. */
  max?: number;
  /** Server-side statement timeout in milliseconds. Default 30 s. */
  statementTimeoutMs?: number;
  /**
   * Called when an IDLE pooled connection fails, such as a TCP timeout while
   * no query is running. The pool has already discarded that connection and
   * the next query opens a new one. Default: a one-line warning on stderr.
   */
  onIdleError?: (error: ChainDataError) => void;
}

/** Postgres `timestamp without time zone`. */
const TIMESTAMP_OID = 1114;

/**
 * db-sync stores UTC in `timestamp without time zone` columns, and `pg` parses
 * those as local time, which shifts every timestamp by the host's offset. This
 * parses them as UTC, for this pool only.
 */
const utcTypes = {
  getTypeParser: ((oid: number, format?: 'text' | 'binary') =>
    oid === TIMESTAMP_OID && format !== 'binary'
      ? (value: string) => new Date(`${value.replace(' ', 'T')}Z`)
      : types.getTypeParser(oid, format)) as typeof types.getTypeParser,
};

const warnIdleError = (error: ChainDataError) => {
  const code = (error.cause as { code?: unknown } | undefined)?.code;
  console.warn(`db-sync: an idle connection failed${typeof code === 'string' ? ` (${code})` : ''}; it was discarded`);
};

/**
 * A failure on an idle pooled connection surfaces as an `error` event on the
 * pool, not as a rejected query, and an `error` event with no listener
 * terminates the process. Nothing is lost by handling it: `pg` has already
 * removed the broken client.
 */
export function handleIdleErrors(
  pool: { on(event: 'error', listener: (error: Error) => void): unknown },
  onIdleError: (error: ChainDataError) => void = warnIdleError,
): void {
  pool.on('error', (error) => onIdleError(fromDatabaseError(error)));
}

export function createPgDb(options: PgDbOptions): ClosableDb {
  const { statementTimeoutMs = 30_000, max = 20, onIdleError, ...connection } = options;
  const config: PoolConfig = {
    ...connection,
    max,
    idleTimeoutMillis: 10_000,
    // Detects a dead peer on an idle connection instead of waiting on it.
    keepAlive: true,
    statement_timeout: statementTimeoutMs,
    types: utcTypes,
  };
  const pool = new Pool(config);
  handleIdleErrors(pool, onIdleError);
  return {
    query: async <T>(sql: string, params: readonly unknown[] = []) => {
      try {
        return (await pool.query(sql, params as unknown[])).rows as T[];
      } catch (error) {
        throw fromDatabaseError(error);
      }
    },
    end: () => pool.end(),
  };
}

/** Wrap a caller-owned Db so its failures are mapped the same way. */
export function guardDb(db: Db): Db {
  return {
    query: async <T>(sql: string, params: readonly unknown[] = []) => {
      try {
        return await db.query<T>(sql, params);
      } catch (error) {
        throw fromDatabaseError(error);
      }
    },
  };
}
