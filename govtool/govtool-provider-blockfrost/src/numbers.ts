import { internal } from './errors';

/** Lovelace and other big integers travel as base-10 strings (SPEC.md §3.2). */
export function toLovelace(value: unknown, what: string): string {
  const text = typeof value === 'number' ? (Number.isSafeInteger(value) ? String(value) : '') : String(value ?? '');
  if (!/^\d+$/.test(text)) throw internal(`Blockfrost sent ${what} that is not a lovelace integer`);
  return BigInt(text).toString();
}

/** An integer Blockfrost may send as a number or a decimal string. */
export function toInt(value: unknown, what: string): number {
  const n = typeof value === 'string' && /^-?\d+$/.test(value) ? Number(value) : value;
  if (typeof n !== 'number' || !Number.isSafeInteger(n)) throw internal(`Blockfrost sent ${what} that is not an integer`);
  return n;
}

/** Unix seconds (Blockfrost's `block_time`) as ISO-8601 UTC. */
export const isoFromUnix = (seconds: number): string => new Date(seconds * 1000).toISOString().replace('.000Z', 'Z');

export const big = (value: string | null | undefined): bigint => (value ? BigInt(value) : 0n);
