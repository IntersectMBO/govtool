/**
 * Coercions from Blockfrost's representations to the contract's.
 *
 * Blockfrost already returns lovelace as decimal strings, so those pass
 * through unchanged — no precision is lost anywhere in this provider.
 */

/** Blockfrost timestamps are POSIX seconds. */
export function toIsoFromUnixSeconds(seconds: number): string {
  return new Date(seconds * 1000).toISOString();
}

export function toNullableIsoFromUnixSeconds(
  seconds: number | null | undefined,
): string | null {
  return seconds === null || seconds === undefined
    ? null
    : toIsoFromUnixSeconds(seconds);
}

export function asFiniteNumber(value: unknown): number | undefined {
  return typeof value === 'number' && Number.isFinite(value)
    ? value
    : undefined;
}

/** A lovelace-ish field: already a string, or a number to stringify. */
export function asLovelace(value: unknown): string | undefined {
  if (typeof value === 'string' && value !== '') return value;
  if (typeof value === 'number' && Number.isFinite(value)) return String(value);
  return undefined;
}

export function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}
