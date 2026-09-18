import { invalidInput } from './errors';

const HEX = /^[0-9a-fA-F]+$/;

export function isHexText(value: string): boolean {
  return HEX.test(value) && value.length % 2 === 0;
}

export function assertHexText(value: string): void {
  if (!isHexText(value)) {
    throw invalidInput('Not a valid hex value', { value });
  }
}

/**
 * Strips the `\x` prefix db-sync's `bytea` rendering leaves on some Koios
 * fields — `tx_info.certificates[].info.meta_hash` comes back as
 * `"\\x297cb7…"` while the same hash on `/drep_updates` comes back bare.
 */
export function stripByteaPrefix(value: string): string {
  return value.startsWith('\\x') ? value.slice(2) : value;
}
