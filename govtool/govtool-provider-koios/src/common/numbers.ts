/**
 * Coercions for what Koios puts on the wire.
 *
 * Koios is PostgREST over db-sync, so `bigint`/`numeric` columns arrive as
 * JSON strings and `int` columns as JSON numbers — the same split `pg` makes,
 * but already through `JSON.parse`. Timestamps are Unix seconds, never ISO.
 */

import { internal } from './errors';

type KoiosNumber = number | string;

/** Lovelace and other big integers travel as decimal strings, unchanged. */
export function toLovelace(value: KoiosNumber): string {
  return typeof value === 'string' ? value : String(value);
}

export function toNullableLovelace(
  value: KoiosNumber | null | undefined,
): string | null {
  return value === null || value === undefined ? null : toLovelace(value);
}

export function toInteger(value: KoiosNumber): number {
  return Math.floor(Number(value));
}

export function toNullableInteger(
  value: KoiosNumber | null | undefined,
): number | null {
  return value === null || value === undefined ? null : toInteger(value);
}

export function toStrictInteger(value: KoiosNumber | null): number {
  const parsed = Number(value);
  if (!Number.isInteger(parsed)) {
    throw internal('Unexpected non-integer value returned from Koios.');
  }
  return parsed;
}

/**
 * Koios reports every point in time as `block_time` — Unix **seconds**, not
 * milliseconds. Multiplying is the whole conversion, and getting it wrong is
 * silent (1970 vs. today), so it lives in one place.
 */
export function toIsoString(unixSeconds: number | string): string {
  const seconds = Number(unixSeconds);
  if (!Number.isFinite(seconds)) {
    throw internal('Unexpected non-numeric block_time returned from Koios.');
  }
  return new Date(seconds * 1000).toISOString();
}

export function toNullableIsoString(
  unixSeconds: number | string | null | undefined,
): string | null {
  return unixSeconds === null || unixSeconds === undefined
    ? null
    : toIsoString(unixSeconds);
}

/**
 * Sums decimal strings without going through `number`.
 *
 * Every total here can exceed `Number.MAX_SAFE_INTEGER` — the DRep
 * distribution is already ~1.5e16 lovelace — so the account-balance
 * reconstruction in `accounts` does its arithmetic in `bigint`.
 */
export function sumLovelace(
  ...values: (KoiosNumber | null | undefined)[]
): string {
  let total = 0n;
  for (const value of values) {
    if (value === null || value === undefined || value === '') {
      continue;
    }
    total += BigInt(value);
  }
  return String(total);
}
