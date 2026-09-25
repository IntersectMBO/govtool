/**
 * Blockfrost's `governance_description` is the ledger's JSON rendering of an
 * action, whose numbers can exceed a float's exact range (lovelace) or carry
 * decimal text a float would reshape. Proposal records are therefore read as
 * text and parsed here, keeping every number's source literal.
 */

/** A JSON number as written in the source. */
export class JsonNumber {
  constructor(readonly text: string) {}
  toNumber(): number {
    return Number(this.text);
  }
}

export type ExactJson = null | boolean | string | JsonNumber | ExactJson[] | { [key: string]: ExactJson };

type Reviver = (this: unknown, key: string, value: unknown, context?: { source?: string }) => unknown;

/**
 * Parse keeping number literals. Node 21+ hands the reviver the source text;
 * on older runtimes the float's shortest rendering is the fallback, which is
 * exact for every integer up to 2^53 and every short decimal.
 */
export function parseExactJson(text: string): ExactJson {
  const reviver: Reviver = (_key, value, context) =>
    typeof value === 'number' ? new JsonNumber(context?.source ?? String(value)) : value;
  return JSON.parse(text, reviver as Parameters<typeof JSON.parse>[1]) as ExactJson;
}

export const isObject = (v: ExactJson | undefined): v is { [key: string]: ExactJson } =>
  typeof v === 'object' && v !== null && !Array.isArray(v) && !(v instanceof JsonNumber);

export const asArray = (v: ExactJson | undefined): ExactJson[] | undefined => (Array.isArray(v) ? v : undefined);

export const asString = (v: ExactJson | undefined): string | undefined => (typeof v === 'string' ? v : undefined);

/** The number's literal, for exact integer / decimal handling. */
export const numText = (v: ExactJson | undefined): string | undefined =>
  v instanceof JsonNumber ? v.text : typeof v === 'string' && /^-?\d+(\.\d+)?$/.test(v) ? v : undefined;

/** Plain JSON again, numbers as numbers where safe and as strings where not. */
export function toPlain(v: ExactJson): unknown {
  if (v instanceof JsonNumber) {
    const n = v.toNumber();
    return /^-?\d+$/.test(v.text) && !Number.isSafeInteger(n) ? v.text : n;
  }
  if (Array.isArray(v)) return v.map(toPlain);
  if (isObject(v)) return Object.fromEntries(Object.entries(v).map(([k, x]) => [k, toPlain(x)]));
  return v;
}
