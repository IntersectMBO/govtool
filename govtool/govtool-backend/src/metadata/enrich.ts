import type { MetadataServiceV1 } from '@govtool/data-providers/metadata';

/**
 * Fills the legacy response fields that come from anchored documents (a DRep's
 * CIP-119 name and profile, a governance action's CIP-108 text) by resolving
 * the anchor through the metadata service. Chain data never resolves a URL;
 * this is the backend doing it on the consumer side.
 *
 * Best effort by design: a document that cannot be resolved within the budget
 * leaves the fields null, exactly as before, and the metadata service keeps
 * fetching and caches it by hash, so the next request is a cache hit.
 */

/** Per-document wait. Cached documents answer in milliseconds. */
export const ENRICH_TIMEOUT_MS = 4000;

/** Documents resolved at once for one page. */
export const ENRICH_CONCURRENCY = 8;

export type Anchor = { url: string | null; hash: string | null };

type Json = Record<string, unknown>;

const isObject = (value: unknown): value is Json =>
  value !== null && typeof value === 'object' && !Array.isArray(value);

/** A field's value, unwrapping a JSON-LD `{"@value": ...}` wrapper. */
const unwrap = (value: unknown): unknown =>
  isObject(value) && '@value' in value ? value['@value'] : value;

const text = (object: unknown, key: string): string | null => {
  if (!isObject(object)) return null;
  const value = unwrap(object[key]);
  return typeof value === 'string' && value.trim() !== '' ? value : null;
};

/** The document's `body`, or undefined when it cannot be had in time. */
export async function resolveBody(
  service: MetadataServiceV1 | null,
  anchor: Anchor,
  timeoutMs = ENRICH_TIMEOUT_MS,
): Promise<Json | undefined> {
  if (!service || !anchor.url || !anchor.hash) return undefined;
  let timer: NodeJS.Timeout | undefined;
  const timeout = new Promise<undefined>((resolve) => {
    timer = setTimeout(() => resolve(undefined), timeoutMs);
  });
  try {
    const result = await Promise.race([
      service
        .getMetadata(anchor.hash.toLowerCase(), anchor.url)
        .catch(() => undefined),
      timeout,
    ]);
    if (!result || !result.ok) return undefined;
    const body = isObject(result.body) ? result.body['body'] : undefined;
    return isObject(body) ? body : undefined;
  } finally {
    clearTimeout(timer);
  }
}

/** Map with at most `limit` promises in flight, preserving order. */
export async function mapLimit<T, R>(
  items: readonly T[],
  limit: number,
  fn: (item: T) => Promise<R>,
): Promise<R[]> {
  const out = new Array<R>(items.length);
  let next = 0;
  const worker = async () => {
    while (next < items.length) {
      const i = next++;
      out[i] = await fn(items[i]);
    }
  };
  await Promise.all(
    Array.from({ length: Math.min(limit, items.length) }, worker),
  );
  return out;
}

type Reference = { '@type'?: string; label?: string; uri?: string };

const references = (body: Json, kind: 'Identity' | 'Link'): Reference[] => {
  const raw = unwrap(body['references']);
  if (!Array.isArray(raw)) return [];
  return raw
    .filter(isObject)
    .map((ref) => ({
      '@type': text(ref, '@type') ?? undefined,
      label: text(ref, 'label') ?? undefined,
      uri: text(ref, 'uri') ?? undefined,
    }))
    .filter((ref) => ref['@type'] === kind);
};

/** The CIP-119 fields of a DRep document, in the legacy response's names. */
export function drepFields(body: Json | undefined) {
  if (!body) return undefined;
  const image = unwrap(body['image']);
  return {
    paymentAddress: text(body, 'paymentAddress'),
    givenName: text(body, 'givenName'),
    objectives: text(body, 'objectives'),
    motivations: text(body, 'motivations'),
    qualifications: text(body, 'qualifications'),
    imageUrl: isObject(image) ? text(image, 'contentUrl') : null,
    imageHash: isObject(image) ? text(image, 'sha256') : null,
    identityReferences: references(body, 'Identity'),
    linkReferences: references(body, 'Link'),
  };
}

/** The CIP-108 fields of a governance action document. */
export function proposalFields(body: Json | undefined) {
  if (!body) return undefined;
  return {
    title: text(body, 'title'),
    abstract: text(body, 'abstract'),
    motivation: text(body, 'motivation'),
    rationale: text(body, 'rationale'),
  };
}
