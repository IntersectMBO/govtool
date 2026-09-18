import type {
  Anchor,
  ConstitutionBody,
  DRepMetadataBody,
  GovActionMetadataBody,
  MetadataAuthor,
  MetadataProjection,
  MetadataReference,
  MetadataStandard,
  MetadataStatus,
  VoteRationaleBody,
} from '@govtool/data-providers/metadata';

import { computeMetadataId } from '../common/metadata-id';
import { stripByteaPrefix } from '../common/hex';

/**
 * Koios is the only surveyed provider that resolves off-chain metadata *and*
 * says whether it validated: `/drep_metadata` and `/proposal_list` carry
 * `meta_json` already flattened out of JSON-LD, plus `is_valid`, `warning`,
 * `comment` and `language`.
 *
 * That makes `MetadataStatus` a read rather than a guess, which is the one
 * place this provider is strictly better informed than db-sync:
 *
 *   `is_valid === false`        → `invalid`   (Koios fetched it and rejected it)
 *   `is_valid === true`         → `valid`
 *   body present, flag absent   → `valid`     (stored implies hash matched)
 *   anchor only, warning set    → `unavailable`
 *   anchor only                 → `pending`
 *
 * The contract still forbids this provider from *fetching* anything: every
 * field here comes out of the same response as the chain data, and a consumer
 * that wants provenance or a forced refresh goes to the Metadata Service.
 */
export interface KoiosMetadataInput {
  url: string | null;
  hash: string | null;
  json: unknown;
  isValid?: boolean | null;
  warning?: string | null;
  comment?: string | null;
}

export function projectGovActionMetadata(
  input: KoiosMetadataInput,
): MetadataProjection<GovActionMetadataBody> | null {
  return project(input, 'CIP108', toGovActionBody);
}

export function projectDRepMetadata(
  input: KoiosMetadataInput,
): MetadataProjection<DRepMetadataBody> | null {
  return project(input, 'CIP119', toDRepBody);
}

export function projectVoteRationale(
  input: KoiosMetadataInput,
): MetadataProjection<VoteRationaleBody> | null {
  return project(input, 'CIP100', toVoteRationaleBody);
}

export function projectConstitution(
  input: KoiosMetadataInput,
): MetadataProjection<ConstitutionBody> | null {
  return project(input, 'CIP100', toConstitutionBody);
}

function project<TBody extends object>(
  input: KoiosMetadataInput,
  standard: MetadataStandard,
  toBody: (json: Record<string, unknown>) => TBody,
): MetadataProjection<TBody> | null {
  if (input.url === null || input.url === undefined) {
    return null;
  }
  const anchor: Anchor = {
    url: input.url,
    dataHash: input.hash === null ? '' : stripByteaPrefix(input.hash),
  };

  const json = asRecord(input.json);
  const body = json === null ? undefined : toBody(json);
  const hasBody = body !== undefined && Object.keys(body).length > 0;
  const failureMessage = input.warning ?? input.comment ?? undefined;

  const projection: MetadataProjection<TBody> = {
    id: computeMetadataId(anchor),
    anchor,
    standard,
    status: deriveStatus(hasBody, input.isValid, failureMessage),
  };
  if (body !== undefined) {
    projection.body = body;
  }
  if (failureMessage !== undefined && failureMessage !== null) {
    projection.failureMessage = failureMessage;
  }
  return projection;
}

export function deriveStatus(
  hasBody: boolean,
  isValid: boolean | null | undefined,
  failureMessage: string | null | undefined,
): MetadataStatus {
  if (isValid === false) return 'invalid';
  if (hasBody) return 'valid';
  if (failureMessage) return 'unavailable';
  return 'pending';
}

/* ------------------------------------------------------------------------- */
/* Body shapes                                                                */
/* ------------------------------------------------------------------------- */

/**
 * CIP-108/100 documents nest their content under `body`; Koios passes that
 * through unchanged, so the projection reads `meta_json.body` and falls back
 * to the document root for the small number of anchors that omit the wrapper.
 */
function documentBody(json: Record<string, unknown>): Record<string, unknown> {
  const body = asRecord(json['body']);
  return body ?? json;
}

function toGovActionBody(json: Record<string, unknown>): GovActionMetadataBody {
  const body = documentBody(json);
  return defined<GovActionMetadataBody>({
    title: asString(body['title']),
    abstract: asString(body['abstract']),
    motivation: asString(body['motivation']),
    rationale: asString(body['rationale']),
    references: toReferences(body['references']),
    authors: toAuthors(json['authors'] ?? body['authors']),
  });
}

function toVoteRationaleBody(json: Record<string, unknown>): VoteRationaleBody {
  const body = documentBody(json);
  return defined<VoteRationaleBody>({
    summary: asString(body['summary']),
    rationaleStatement: asString(body['rationaleStatement']),
    precedentDiscussion: asString(body['precedentDiscussion']),
    counterargumentDiscussion: asString(body['counterargumentDiscussion']),
    conclusion: asString(body['conclusion']),
    internalVote: asNumberRecord(body['internalVote']),
    references: toReferences(body['references']),
    authors: toAuthors(json['authors'] ?? body['authors']),
  });
}

function toConstitutionBody(json: Record<string, unknown>): ConstitutionBody {
  const body = documentBody(json);
  return defined<ConstitutionBody>({
    text: asString(body['text'] ?? body['constitution']),
    contentType: asString(body['contentType']),
  });
}

/**
 * CIP-119 splits its references into identity and link lists, but the
 * documents in the wild — and Koios' flattening of them — usually carry one
 * `references` array discriminated by `@type`. Both are read, and an
 * un-typed entry is treated as a link, which is what it renders as.
 */
function toDRepBody(json: Record<string, unknown>): DRepMetadataBody {
  const body = documentBody(json);
  const all = toReferences(body['references']) ?? [];
  const identity = toReferences(body['identityReferences']) ?? [];
  const link = toReferences(body['linkReferences']) ?? [];

  for (const ref of all) {
    (ref['@type'] === 'Identity' ? identity : link).push(ref);
  }

  return defined<DRepMetadataBody>({
    givenName: asString(body['givenName']),
    objectives: asString(body['objectives']),
    motivations: asString(body['motivations']),
    qualifications: asString(body['qualifications']),
    paymentAddress: asString(body['paymentAddress']),
    image: toImage(body['image']),
    identityReferences: identity.length > 0 ? identity : undefined,
    linkReferences: link.length > 0 ? link : undefined,
    doNotList: asBoolean(body['doNotList']),
  });
}

function toImage(value: unknown): DRepMetadataBody['image'] {
  const image = asRecord(value);
  if (image === null) {
    const url = asString(value);
    return url === undefined ? undefined : { url };
  }
  const projected = defined<NonNullable<DRepMetadataBody['image']>>({
    url: asString(image['contentUrl'] ?? image['url']),
    contentHash: asString(image['sha256'] ?? image['contentHash']),
    data: asString(image['data']),
  });
  return Object.keys(projected).length > 0 ? projected : undefined;
}

function toReferences(value: unknown): MetadataReference[] | undefined {
  if (!Array.isArray(value)) return undefined;
  const refs: MetadataReference[] = [];
  for (const entry of value) {
    const ref = asRecord(entry);
    if (ref === null) continue;
    const uri = asString(ref['uri'] ?? ref['@id']);
    if (uri === undefined) continue;
    const type = asString(ref['@type']);
    refs.push({
      '@type':
        type === 'Identity' || type === 'Link' || type === 'Other'
          ? type
          : 'Other',
      label: asString(ref['label']) ?? '',
      uri,
    });
  }
  return refs.length > 0 ? refs : undefined;
}

function toAuthors(value: unknown): MetadataAuthor[] | undefined {
  if (!Array.isArray(value)) return undefined;
  const authors: MetadataAuthor[] = [];
  for (const entry of value) {
    const author = asRecord(entry);
    if (author === null) continue;
    const witness = asRecord(author['witness']);
    authors.push(
      defined<MetadataAuthor>({
        name: asString(author['name']),
        witnessAlgorithm: asString(witness?.['witnessAlgorithm']),
        publicKey: asString(witness?.['publicKey']),
        signature: asString(witness?.['signature']),
      }),
    );
  }
  return authors.length > 0 ? authors : undefined;
}

/* ------------------------------------------------------------------------- */
/* Coercions — every one of these can meet a hand-written JSON-LD document    */
/* ------------------------------------------------------------------------- */

export function asRecord(value: unknown): Record<string, unknown> | null {
  return typeof value === 'object' && value !== null && !Array.isArray(value)
    ? (value as Record<string, unknown>)
    : null;
}

function asString(value: unknown): string | undefined {
  if (typeof value === 'string') return value;
  // Some documents keep the JSON-LD `{"@value": "…"}` wrapper Koios did not flatten.
  const wrapped = asRecord(value)?.['@value'];
  return typeof wrapped === 'string' ? wrapped : undefined;
}

function asBoolean(value: unknown): boolean | undefined {
  return typeof value === 'boolean' ? value : undefined;
}

function asNumberRecord(value: unknown): Record<string, number> | undefined {
  const record = asRecord(value);
  if (record === null) return undefined;
  const out: Record<string, number> = {};
  for (const [key, entry] of Object.entries(record)) {
    if (typeof entry === 'number') out[key] = entry;
  }
  return Object.keys(out).length > 0 ? out : undefined;
}

/** Drops the `undefined` keys, so an absent field stays absent in the body. */
function defined<T extends object>(fields: {
  [K in keyof T]: T[K] | undefined;
}): T {
  const out: Record<string, unknown> = {};
  for (const [key, value] of Object.entries(fields)) {
    if (value !== undefined) out[key] = value;
  }
  return out as T;
}
