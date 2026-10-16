import { internal } from './errors';

type WireNumber = number | string | bigint;

/**
 * Lovelace and other big integers travel as base-10 strings (SPEC.md §3.2).
 * Koios sends most of them as strings already; the HTTP client keeps any
 * integer literal beyond 2^53 as its source text, so nothing here rounds.
 */
export function toLovelace(value: WireNumber, what = 'lovelace'): string {
  const text = typeof value === 'string' ? value.trim().replace(/\.0+$/, '') : typeof value === 'bigint' ? value.toString() : String(value);
  if (!/^\d+$/.test(text)) throw internal(`Koios sent a non-integer ${what}`, { value: String(value) });
  return BigInt(text).toString();
}

export const toNullableLovelace = (value: WireNumber | null | undefined, what?: string): string | null =>
  value === null || value === undefined ? null : toLovelace(value, what);

export function toInt(value: WireNumber, what = 'integer'): number {
  const n = typeof value === 'number' ? value : Number(value);
  if (!Number.isSafeInteger(n)) throw internal(`Koios sent a non-integer ${what}`, { value: String(value) });
  return n;
}

export const toNullableInt = (value: WireNumber | null | undefined, what?: string): number | null =>
  value === null || value === undefined ? null : toInt(value, what);

/** A Koios UNIX timestamp (seconds) as ISO-8601 UTC. */
export const toIso = (unixSeconds: number): string => new Date(unixSeconds * 1000).toISOString().replace('.000Z', 'Z');

/** Koios leaks PostgreSQL's bytea escape on a few hash columns (`\\xca7d…`). */
export const stripBytea = (hex: string): string => hex.replace(/^\\x/, '').toLowerCase();

export const sumLovelace = (values: readonly string[]): string => values.reduce((a, b) => a + BigInt(b), 0n).toString();
