type DbNumber = number | string | bigint;

/** Lovelace and other big integers travel as base-10 strings (SPEC.md §3.2). */
export const toLovelace = (value: DbNumber): string =>
  typeof value === 'string' ? value.replace(/\.0+$/, '') : String(value);

export const toNullableLovelace = (value: DbNumber | null | undefined): string | null =>
  value === null || value === undefined ? null : toLovelace(value);

export const toInt = (value: DbNumber): number => Math.trunc(Number(value));

export const toNullableInt = (value: DbNumber | null | undefined): number | null =>
  value === null || value === undefined ? null : toInt(value);

/** A pg timestamp (Date, or string when a query casts it) as ISO-8601 UTC. */
export const toIso = (value: Date | string): string =>
  value instanceof Date ? value.toISOString() : new Date(`${value}${/Z|[+-]\d\d:?\d\d$/.test(value) ? '' : 'Z'}`).toISOString();

export const toHex = (value: Buffer | Uint8Array | string): string =>
  typeof value === 'string' ? value.replace(/^\\x/, '').toLowerCase() : Buffer.from(value).toString('hex');
