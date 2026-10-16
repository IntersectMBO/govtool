import { dbInteger, type ApiInteger } from './integer';

/**
 * Coercions from the contract's representations back to the ones the legacy
 * GovTool API returns on the wire.
 *
 * The contract carries lovelace as a decimal string, because total supply
 * (4.5e16) exceeds `Number.MAX_SAFE_INTEGER` (9.0e15). The legacy API emits
 * JSON numbers, and those stay numbers — but not by rounding: a lovelace
 * field goes through `dbInteger`, which keeps safe values as numbers and
 * larger ones as bigint, and the response interceptor writes a bigint to JSON
 * unquoted. So the wire format is unchanged and nothing is lost.
 *
 * `toLegacyNumber` remains the right coercion for values that are counts,
 * epochs or indexes, where the safe range is not in question.
 */

/** `Math.floor(Number(value))`, the legacy default. */
export function toLegacyNumber(value: string | number): number {
  return Math.floor(Number(value));
}

/** Exact, for nullable lovelace fields; see `dbInteger`. */
export function toLegacyNullableInteger(
  value: string | number | null | undefined,
): ApiInteger | null {
  return value === null || value === undefined ? null : dbInteger(value);
}

export function toLegacyNullableNumber(
  value: string | number | null | undefined,
): number | null {
  return value === null || value === undefined ? null : toLegacyNumber(value);
}

/** Legacy fields are `null`, never absent, so `undefined` collapses to `null`. */
export function orNull<T>(value: T | undefined | null): T | null {
  return value === undefined || value === null ? null : value;
}
