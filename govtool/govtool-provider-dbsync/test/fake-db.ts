import { loadSql, type SqlFileName } from '../src/db/sql-loader';
import type { Queryable, QueryResultLike } from '../src/db/queryable';

export interface RecordedCall {
  file: SqlFileName | 'unknown';
  params: readonly unknown[];
  mode: 'object' | 'array';
}

/**
 * A `Queryable` that answers from canned rows keyed by SQL file, and records
 * what it was asked. Matching is by the statement's text, so a test proves
 * the provider ran the file it claims to, with the parameters it claims to —
 * which is the part that has to stay identical to the legacy backend.
 */
export class FakeDb implements Queryable {
  readonly calls: RecordedCall[] = [];
  private readonly objectRows = new Map<SqlFileName, unknown[]>();
  private readonly arrayRows = new Map<SqlFileName, unknown[][]>();
  private readonly failures = new Map<SqlFileName, Error>();
  private readonly textToFile: Map<string, SqlFileName>;

  constructor(files: readonly SqlFileName[]) {
    this.textToFile = new Map(files.map((file) => [loadSql(file), file]));
  }

  on(file: SqlFileName, rows: unknown[]): this {
    this.objectRows.set(file, rows);
    return this;
  }

  onArrays(file: SqlFileName, rows: unknown[][]): this {
    this.arrayRows.set(file, rows);
    return this;
  }

  failOn(file: SqlFileName, error: Error): this {
    this.failures.set(file, error);
    return this;
  }

  callsTo(file: SqlFileName): RecordedCall[] {
    return this.calls.filter((call) => call.file === file);
  }

  query<T>(
    sql: string,
    params: readonly unknown[] = [],
  ): Promise<QueryResultLike<T>> {
    const file = this.resolve(sql);
    this.calls.push({ file, params, mode: 'object' });
    const failure = this.failures.get(file as SqlFileName);
    if (failure) {
      return Promise.reject(failure);
    }
    return Promise.resolve({
      rows: (this.objectRows.get(file as SqlFileName) ?? []) as T[],
    });
  }

  queryArrays<T extends unknown[]>(
    sql: string,
    params: readonly unknown[] = [],
  ): Promise<QueryResultLike<T>> {
    const file = this.resolve(sql);
    this.calls.push({ file, params, mode: 'array' });
    const failure = this.failures.get(file as SqlFileName);
    if (failure) {
      return Promise.reject(failure);
    }
    return Promise.resolve({
      rows: (this.arrayRows.get(file as SqlFileName) ?? []) as T[],
    });
  }

  private resolve(sql: string): SqlFileName | 'unknown' {
    return this.textToFile.get(sql) ?? 'unknown';
  }
}

export function fakeDb(...files: SqlFileName[]): FakeDb {
  return new FakeDb(files);
}

/** A minimal `list-dreps.sql` row; override only what a test cares about. */
export function drepListRow(
  overrides: Record<string, unknown> = {},
): Record<string, unknown> {
  return {
    drep_hash: 'a'.repeat(56),
    view: 'drep1exampleview',
    has_script: false,
    url: 'https://example.com/drep.jsonld',
    metadata_hash: 'b'.repeat(64),
    deposit: '500000000',
    amount: '12500000000',
    active: true,
    tx_hash: 'c'.repeat(64),
    last_register_time: new Date('2026-01-01T00:00:00.000Z'),
    latest_deposit: '500000000',
    has_non_deregister_voting_anchor: false,
    fetch_error: null,
    payment_address: 'stake1example',
    given_name: 'Example DRep',
    objectives: 'Objectives',
    motivations: null,
    qualifications: null,
    image_url: null,
    image_hash: null,
    votes_last_year: '7',
    identity_references: [],
    link_references: [],
    ...overrides,
  };
}

/** A minimal `list-proposals.sql` row. */
export function proposalRow(
  overrides: Record<string, unknown> = {},
): Record<string, unknown> {
  return {
    id: '42',
    tx_hash: 'd'.repeat(64),
    index: 0,
    type: 'InfoAction',
    description: { data: {} },
    expiry_date: new Date('2026-03-01T00:00:00.000Z'),
    expiration: 510,
    time: new Date('2026-01-05T00:00:00.000Z'),
    epoch_no: 500,
    url: 'https://example.com/ga.jsonld',
    data_hash: 'e'.repeat(64),
    proposal_params: null,
    title: 'A title',
    abstract: 'An abstract',
    motivation: null,
    rationale: null,
    yes_votes: '1000000',
    no_votes: '2000000',
    abstain_votes: '3000000',
    pool_yes_votes: '4000000',
    pool_no_votes: '0',
    pool_abstain_votes: '0',
    cc_yes_votes: '3',
    cc_no_votes: '1',
    cc_abstain_votes: '0',
    prev_gov_action_index: null,
    prev_gov_action_tx_hash: null,
    json_content: null,
    authors: [],
    ...overrides,
  };
}

/** A minimal `get-drep-info.sql` row. */
export function drepInfoRow(
  overrides: Record<string, unknown> = {},
): Record<string, unknown> {
  return {
    is_script_based: false,
    is_registered_as_drep: true,
    was_registered_as_drep: true,
    is_registered_as_sole_voter: false,
    was_registered_as_sole_voter: false,
    deposit: '500000000',
    url: 'https://example.com/drep.jsonld',
    data_hash: 'b'.repeat(64),
    voting_power: '12500000000',
    drep_register_tx_hash: 'c'.repeat(64),
    drep_retire_tx_hash: null,
    sole_voter_register_tx_hash: null,
    sole_voter_retire_tx_hash: null,
    payment_address: 'stake1example',
    given_name: 'Example DRep',
    objectives: null,
    motivations: null,
    qualifications: null,
    image_url: null,
    image_hash: null,
    ...overrides,
  };
}
