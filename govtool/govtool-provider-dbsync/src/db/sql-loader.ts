import * as fs from 'node:fs';
import * as path from 'node:path';

/**
 * The SQL this provider runs, byte-for-byte the files the legacy GovTool
 * backend ships in `backend-ts/sql`. They are the specification of what the
 * db-sync read model returns, and they are not edited here: when a contract
 * field cannot be filled from them, the contract loosens, the SQL does not
 * grow. That keeps this provider's results identical to the legacy API's.
 *
 * Typing the names as a union turns a misspelled file into a compile error
 * instead of an `ENOENT` in production.
 */
export type SqlFileName =
  | 'get-account-info.sql'
  | 'get-current-delegation.sql'
  | 'get-current-epoch-params.sql'
  | 'get-drep-info.sql'
  | 'get-dreps-voting-power-list.sql'
  | 'get-filtered-dreps-voting-power.sql'
  | 'get-network-info.sql'
  | 'get-network-metrics.sql'
  | 'get-network-total-stake.sql'
  | 'get-previous-enacted-governance-action-proposal-details.sql'
  | 'get-stake-key-voting-power.sql'
  | 'get-survey-definition.sql'
  | 'get-transaction-status.sql'
  | 'get-votes.sql'
  | 'get-voting-power.sql'
  | 'list-dreps.sql'
  | 'list-proposals.sql';

/** `<package root>/sql`, from either `src/db` or `dist/db`. */
const SQL_DIR = path.resolve(__dirname, '..', '..', 'sql');

const cache = new Map<SqlFileName, string>();

export function loadSql(fileName: SqlFileName): string {
  const cached = cache.get(fileName);
  if (cached !== undefined) {
    return cached;
  }
  const sql = fs.readFileSync(path.join(SQL_DIR, fileName), 'utf8');
  cache.set(fileName, sql);
  return sql;
}

/** Where the SQL is read from — exposed for diagnostics and tests. */
export function sqlDirectory(): string {
  return SQL_DIR;
}
