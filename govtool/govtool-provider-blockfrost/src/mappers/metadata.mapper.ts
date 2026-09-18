import type {
  Anchor,
  MetadataProjection,
  MetadataStandard,
} from '@govtool/data-providers/metadata';
import * as blake from 'blakejs';

import { isRecord } from '../common/numbers';

/**
 * `MetadataId` exactly as the contract defines it:
 * `blake2b-256( utf8(url) ‖ 0x23 ‖ bytes(dataHash) )`, hex.
 *
 * Kept byte-identical to the db-sync provider's implementation so the same
 * anchor gets the same id whichever provider produced it — that is the whole
 * point of the id being deterministic.
 */
export function computeMetadataId(anchor: Anchor): string {
  const hash =
    /^[0-9a-fA-F]*$/.test(anchor.dataHash) && anchor.dataHash.length % 2 === 0
      ? Buffer.from(anchor.dataHash, 'hex')
      : Buffer.from(anchor.dataHash, 'utf8');

  return blake.blake2bHex(
    Buffer.concat([Buffer.from(anchor.url, 'utf8'), Buffer.from([0x23]), hash]),
    undefined,
    32,
  );
}

/**
 * Blockfrost resolves and parses the off-chain document itself and returns it
 * as `json_metadata`, so a successful metadata response means the document
 * was fetched. It does **not** say whether the hash matched or whether the
 * document conforms to its CIP, and Blockfrost exposes no retrieval-failure
 * record at all — a document it could not fetch is simply a 404.
 *
 * So status is `valid` when a body came back and `pending` when the anchor
 * exists but no document did. `invalid` and `unavailable` are never reported:
 * this provider cannot distinguish them, and claiming either would be a
 * stronger statement than Blockfrost supports. A consumer that needs real
 * validation calls the metadata service.
 */
export function projectMetadata<TBody extends object>(input: {
  url: string;
  hash: string;
  standard: MetadataStandard;
  jsonMetadata: unknown;
}): MetadataProjection<TBody> {
  const anchor: Anchor = { url: input.url, dataHash: input.hash };
  const body = extractBody<TBody>(input.jsonMetadata);

  const projection: MetadataProjection<TBody> = {
    id: computeMetadataId(anchor),
    anchor,
    standard: input.standard,
    status: body === undefined ? 'pending' : 'valid',
  };
  if (body !== undefined) projection.body = body;
  if (input.jsonMetadata !== null && input.jsonMetadata !== undefined) {
    projection.raw = input.jsonMetadata;
  }
  return projection;
}

/**
 * CIP-100 wraps the payload in a `body` object, and its fields may be plain
 * values or JSON-LD `{ "@value": … }` boxes. Both are unwrapped here, which
 * is what the legacy metadata validator did too.
 */
function extractBody<TBody extends object>(json: unknown): TBody | undefined {
  if (!isRecord(json)) return undefined;
  const raw = isRecord(json.body) ? json.body : json;
  const out: Record<string, unknown> = {};
  let found = false;
  for (const [key, value] of Object.entries(raw)) {
    if (key.startsWith('@')) continue;
    const unwrapped = unwrapValue(value);
    if (unwrapped !== undefined && unwrapped !== null) {
      out[key] = unwrapped;
      found = true;
    }
  }
  return found ? (out as TBody) : undefined;
}

function unwrapValue(value: unknown): unknown {
  if (Array.isArray(value)) return value.map(unwrapValue);
  if (isRecord(value)) {
    if ('@value' in value) return value['@value'];
    const out: Record<string, unknown> = {};
    for (const [k, v] of Object.entries(value)) {
      if (k.startsWith('@') && k !== '@type') continue;
      out[k] = unwrapValue(v);
    }
    return out;
  }
  return value;
}
