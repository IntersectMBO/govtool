/**
 * The one thing this provider needs from a database: run parameterised SQL and
 * get rows back. Declared as an interface so the whole provider can be driven
 * by a fake in tests, and so a consumer that already owns a `pg.Pool` can hand
 * it in instead of letting the provider open a second one.
 */
export interface QueryResultLike<T> {
  rows: T[];
}

export interface Queryable {
  /** Rows as objects keyed by column name — the usual case. */
  query<T = Record<string, unknown>>(
    sql: string,
    params?: readonly unknown[],
  ): Promise<QueryResultLike<T>>;

  /**
   * Rows as positional arrays. Needed for a statement whose unaliased columns
   * collide by name: `get-votes.sql` selects three `encode(...)` expressions
   * and an unaliased `CONCAT`, which the object form silently overwrites.
   */
  queryArrays<T extends unknown[] = unknown[]>(
    sql: string,
    params?: readonly unknown[],
  ): Promise<QueryResultLike<T>>;
}

export interface ClosableQueryable extends Queryable {
  end(): Promise<void>;
}
