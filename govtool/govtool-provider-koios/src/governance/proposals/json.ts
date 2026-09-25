/**
 * Plain JSON as the HTTP client parses it: numbers are numbers, except an
 * integer literal beyond 2^53, which arrives as its exact digit string.
 */
export type ExactJson = null | boolean | string | number | ExactJson[] | { [key: string]: ExactJson };

export const isObject = (v: unknown): v is { [key: string]: ExactJson } =>
  typeof v === 'object' && v !== null && !Array.isArray(v);

export const asArray = (v: unknown): ExactJson[] | undefined => (Array.isArray(v) ? (v as ExactJson[]) : undefined);

export const asString = (v: unknown): string | undefined => (typeof v === 'string' ? v : undefined);

/** A number's text, for exact integer / decimal handling. Digit strings are big integers. */
export function numText(v: unknown): string | undefined {
  if (typeof v === 'number') return Number.isFinite(v) ? String(v) : undefined;
  if (typeof v === 'string' && /^-?\d+(\.\d+)?$/.test(v)) return v;
  return undefined;
}

/** Plain JSON again (already plain; kept for parity with the db-sync decoder). */
export const toPlain = (v: ExactJson): unknown => v;
