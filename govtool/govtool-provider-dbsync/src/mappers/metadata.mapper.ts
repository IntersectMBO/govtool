import type {
  Anchor,
  MetadataProjection,
  MetadataStandard,
  MetadataStatus,
} from '@govtool/data-providers/metadata';

import { computeMetadataId } from '../common/metadata-id';

/**
 * Builds the slim projection the chain read model embeds, from what db-sync
 * has joined onto the row: the anchor, the parsed off-chain fields, and the
 * last fetch error.
 *
 * Status is derived, since db-sync does not store one:
 *   - any parsed field present → `valid` (db-sync only stores off-chain data
 *     whose hash matched the anchor, so "stored" implies "hash ok");
 *   - otherwise a fetch error → `unavailable`;
 *   - otherwise → `pending`.
 * Body and failure text are carried independently of status, so a consumer
 * reconstructing a legacy response never loses a field to the derivation.
 */
export function projectMetadata<TBody extends object>(input: {
  anchor: Anchor;
  standard: MetadataStandard;
  body: TBody;
  hasBodyData: boolean;
  failureMessage?: string | null;
  raw?: unknown;
}): MetadataProjection<TBody> {
  let status: MetadataStatus;
  if (input.hasBodyData) {
    status = 'valid';
  } else if (input.failureMessage) {
    status = 'unavailable';
  } else {
    status = 'pending';
  }

  const projection: MetadataProjection<TBody> = {
    id: computeMetadataId(input.anchor),
    anchor: input.anchor,
    standard: input.standard,
    status,
    body: input.body,
  };
  if (input.failureMessage) {
    projection.failureMessage = input.failureMessage;
  }
  if (input.raw !== undefined && input.raw !== null) {
    projection.raw = input.raw;
  }
  return projection;
}

/** Copies only the non-null fields, so absent data stays absent in the body. */
export function definedFields<T extends Record<string, unknown>>(fields: {
  [K in keyof T]: T[K] | null | undefined;
}): { body: Partial<T>; hasData: boolean } {
  const body: Partial<T> = {};
  let hasData = false;
  for (const key of Object.keys(fields) as (keyof T)[]) {
    const value = fields[key];
    if (value !== null && value !== undefined) {
      body[key] = value as T[keyof T];
      hasData = true;
    }
  }
  return { body, hasData };
}
