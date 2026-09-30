// Readers for writable body fields (SPEC §3.5). Each returns undefined when
// the key is absent, so an endpoint can tell "not sent" from "sent as null".
// Type errors give 400 V `<field> is invalid`; over-length gives V
// `<field> is too long`.

import { validationError } from './errors';
import type { DataPayload } from './body';

const invalid = (f: string) => validationError(`${f} is invalid`);

/**
 * Postgres cannot store U+0000 in text or jsonb; left to the database it
 * would fail the write with a 500. Every string reader refuses it as V.
 */
export function hasNul(s: string): boolean {
  return s.includes('\u0000');
}

/** True when a JSON value holds U+0000 in any string, key included. */
export function hasNulDeep(v: unknown): boolean {
  if (typeof v === 'string') return hasNul(v);
  if (Array.isArray(v)) return v.some(hasNulDeep);
  if (typeof v === 'object' && v !== null) {
    return Object.entries(v).some(([k, x]) => hasNul(k) || hasNulDeep(x));
  }
  return false;
}
const tooLong = (f: string) => validationError(`${f} is too long`);

export interface StringOpts {
  /** Maximum length; default 255 (varchar). Pass Infinity for unbounded text. */
  max?: number;
  /** Accept numbers and stringify them. Default true (varchar rule). */
  acceptNumbers?: boolean;
}

/** string | null | undefined. */
export function readString(d: DataPayload, field: string, opts: StringOpts = {}): string | null | undefined {
  if (!Object.prototype.hasOwnProperty.call(d, field)) return undefined;
  const v = d[field];
  if (v === null || v === undefined) return v ?? undefined;
  let s: string;
  if (typeof v === 'string') s = v;
  else if (typeof v === 'number' && Number.isFinite(v) && opts.acceptNumbers !== false) s = String(v);
  else throw invalid(field);
  if (hasNul(s)) throw invalid(field);
  if (s.length > (opts.max ?? 255)) throw tooLong(field);
  return s;
}

/** Text column: unbounded unless `max` is given; numbers rejected. */
export function readText(d: DataPayload, field: string, max = Infinity): string | null | undefined {
  return readString(d, field, { max, acceptNumbers: false });
}

/** JSON boolean | null | undefined. */
export function readBool(d: DataPayload, field: string): boolean | null | undefined {
  if (!Object.prototype.hasOwnProperty.call(d, field)) return undefined;
  const v = d[field];
  if (v === null || v === undefined) return v ?? undefined;
  if (typeof v !== 'boolean') throw invalid(field);
  return v;
}

/** Integer reference: a number or a decimal string. */
export function readIntRef(d: DataPayload, field: string): number | null | undefined {
  if (!Object.prototype.hasOwnProperty.call(d, field)) return undefined;
  const v = d[field];
  if (v === null || v === undefined) return v ?? undefined;
  return toIntRef(v, field);
}

/** Coerce a number or decimal string to a positive int4, else V `<field> is invalid`. */
export function toIntRef(v: unknown, field: string): number {
  let n: number;
  if (typeof v === 'number') n = v;
  else if (typeof v === 'string' && /^\d+$/.test(v.trim())) n = Number(v.trim());
  else throw invalid(field);
  if (!Number.isSafeInteger(n) || n < 1 || n > 2147483647) throw invalid(field);
  return n;
}

/** Plain JSON object | null | undefined. */
export function readObject(d: DataPayload, field: string): DataPayload | null | undefined {
  if (!Object.prototype.hasOwnProperty.call(d, field)) return undefined;
  const v = d[field];
  if (v === null || v === undefined) return v ?? undefined;
  if (typeof v !== 'object' || Array.isArray(v)) throw invalid(field);
  return v as DataPayload;
}

/** JSON array | null | undefined, with a maximum length. */
export function readArray(d: DataPayload, field: string, max: number): unknown[] | null | undefined {
  if (!Object.prototype.hasOwnProperty.call(d, field)) return undefined;
  const v = d[field];
  if (v === null || v === undefined) return v ?? undefined;
  if (!Array.isArray(v)) throw invalid(field);
  if (v.length > max) throw tooLong(field);
  return v as unknown[];
}

/** A path id (`/:id`): decimal digits only, else null. */
export function parseRouteId(raw: string): number | null {
  if (!/^\d+$/.test(raw)) return null;
  const n = Number(raw);
  return n >= 1 && n <= 2147483647 ? n : null;
}
