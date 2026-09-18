/**
 * Numeric and temporal coercions, each mirroring one the legacy backend
 * applied so that a value round-trips through the contract to the same
 * legacy output.
 *
 * `pg` hands back `bigint`/`numeric` columns as strings and `int` columns as
 * numbers; `timestamp` columns arrive as `Date`. Nothing here assumes which.
 */

import { internal } from './errors';

type DbNumber = number | string | bigint;

/** Lovelace and other big integers travel as decimal strings, unchanged. */
export function toLovelace(value: DbNumber): string {
  return typeof value === 'string' ? value : String(value);
}

export function toNullableLovelace(
  value: DbNumber | null | undefined,
): string | null {
  return value === null || value === undefined ? null : toLovelace(value);
}

/** Legacy default: `Math.floor(Number(value))`. */
export function toInteger(value: DbNumber): number {
  return Math.floor(Number(value));
}

export function toNullableInteger(
  value: DbNumber | null | undefined,
): number | null {
  return value === null || value === undefined ? null : toInteger(value);
}

/**
 * Legacy `NetworkService.toInteger`: anything that is not already an integer
 * is a corrupt read and fails the whole response. `Number(null)` is `0`, and
 * `0` is an integer — that is the legacy behaviour for a `NULL` counter too.
 */
export function toStrictInteger(value: DbNumber | null): number {
  const parsed = Number(value);
  if (!Number.isInteger(parsed)) {
    throw internal('Unexpected non-integer value returned from database.');
  }
  return parsed;
}

/** Legacy `AdaHolderService.toInteger`: unparseable means `0`, never an error. */
export function toLenientInteger(value: DbNumber): number {
  const parsed = Number(value);
  return Number.isFinite(parsed) ? Math.floor(parsed) : 0;
}

export function toIsoString(value: Date | string): string {
  return value instanceof Date
    ? value.toISOString()
    : new Date(value).toISOString();
}

export function toNullableIsoString(
  value: Date | string | null | undefined,
): string | null {
  return value === null || value === undefined ? null : toIsoString(value);
}
