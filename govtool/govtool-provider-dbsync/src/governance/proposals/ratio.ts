import type { Ratio } from '@govtool/data-providers/chain-data';

import { internal } from '../../errors';

/** Denominator cap for the continued-fraction fallback (SPEC.md §3.2). */
export const RATIO_DENOMINATOR_CAP = 1000;

/** A decimal with at most this many fractional digits is taken as written. */
const EXACT_DECIMAL_DIGITS = 9;

const gcd = (a: bigint, b: bigint): bigint => {
  let [x, y] = [a < 0n ? -a : a, b < 0n ? -b : b];
  while (y !== 0n) [x, y] = [y, x % y];
  return x;
};

function reduced(numerator: bigint, denominator: bigint): Ratio | undefined {
  if (denominator === 0n) return undefined;
  const g = gcd(numerator, denominator) || 1n;
  const n = numerator / g;
  const d = denominator / g;
  const max = BigInt(Number.MAX_SAFE_INTEGER);
  if (n > max || d > max || n < -max) return undefined;
  return { numerator: Number(n), denominator: Number(d) };
}

/**
 * Best rational approximation with a bounded denominator, by continued
 * fractions. Recovers `0.6666666666666666 -> 2/3` and `0.67 -> 67/100`.
 */
export function continuedFraction(x: number, cap = RATIO_DENOMINATOR_CAP): Ratio {
  const sign = x < 0 ? -1 : 1;
  let v = Math.abs(x);
  let [h0, h1, k0, k1] = [0, 1, 1, 0];
  for (let i = 0; i < 64; i++) {
    const a = Math.floor(v);
    const h2 = a * h1 + h0;
    const k2 = a * k1 + k0;
    if (k2 > cap) break;
    [h0, h1, k0, k1] = [h1, h2, k1, k2];
    const frac = v - a;
    if (frac < 1e-12) break;
    v = 1 / frac;
  }
  return { numerator: sign * h1, denominator: k1 };
}

const DECIMAL = /^(-?)(\d+)(?:\.(\d+))?(?:[eE]([+-]?\d+))?$/;

/** Parse a base-10 literal exactly, as written. */
function exactDecimal(text: string): { ratio: Ratio; fractionDigits: number } | undefined {
  const m = DECIMAL.exec(text.trim());
  if (!m) return undefined;
  const [, sign, whole, fraction = '', exp = '0'] = m;
  const shift = fraction.length - Number(exp);
  const digits = BigInt(`${sign}${whole}${fraction}`);
  const ratio =
    shift >= 0 ? reduced(digits, 10n ** BigInt(shift)) : reduced(digits * 10n ** BigInt(-shift), 1n);
  return ratio ? { ratio, fractionDigits: Math.max(0, shift) } : undefined;
}

/**
 * A ledger rational from whatever the source holds.
 *
 * - `{ numerator, denominator }` is exact and taken as-is.
 * - A short decimal literal (`0.67`, `0.000050`) is exact as written.
 * - A long one is a float rendering of a rational with no terminating decimal
 *   (`0.6666666666666666`), reconstructed by bounded continued fractions and
 *   accepted only if it reproduces the float; otherwise the exact decimal wins.
 *
 * Returns undefined for anything that is not a finite number.
 */
export function toRatio(value: unknown): Ratio | undefined {
  if (value === null || value === undefined) return undefined;
  if (typeof value === 'object') {
    const { numerator, denominator } = value as { numerator?: unknown; denominator?: unknown };
    if (numerator === undefined || denominator === undefined) return undefined;
    try {
      return reduced(BigInt(String(numerator)), BigInt(String(denominator)));
    } catch {
      return undefined;
    }
  }
  if (typeof value !== 'number' && typeof value !== 'string') return undefined;
  const text = typeof value === 'number' ? String(value) : value;
  const x = Number(text);
  if (!Number.isFinite(x) || !DECIMAL.test(text.trim())) return undefined;
  const exact = exactDecimal(text);
  if (exact && exact.fractionDigits <= EXACT_DECIMAL_DIGITS) return exact.ratio;
  const approx = continuedFraction(x);
  if (Math.abs(approx.numerator / approx.denominator - x) < 1e-12) return approx;
  return exact?.ratio ?? continuedFraction(x, Number.MAX_SAFE_INTEGER);
}

/** As `toRatio`, but a value that must exist and does not is an internal fault. */
export function requireRatio(value: unknown, what: string): Ratio {
  const ratio = toRatio(value);
  if (!ratio) throw internal(`${what} is not a rational`);
  return ratio;
}

/** Larger of two ratios, by cross-multiplication. */
export const maxRatio = (a: Ratio, b: Ratio): Ratio =>
  a.numerator * b.denominator >= b.numerator * a.denominator ? a : b;
