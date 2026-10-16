import { createParamDecorator, ExecutionContext } from '@nestjs/common';
import type { Request } from 'express';
import { validationError } from './errors';

export type DataPayload = Record<string, unknown>;

const isObj = (v: unknown): v is DataPayload => typeof v === 'object' && v !== null && !Array.isArray(v);

/** `body.data` or 400 V `Missing "data" payload in the request body` (§3.5). */
export function unwrapData(body: unknown): DataPayload {
  if (!isObj(body) || !isObj(body.data)) {
    throw validationError('Missing "data" payload in the request body');
  }
  return body.data;
}

/**
 * The `{data: {...}}` body of a write, unwrapped. Read only the writable
 * fields from it; everything else is ignored (§3.5, Δ3).
 */
export const DataBody = createParamDecorator((_: unknown, ctx: ExecutionContext): DataPayload => {
  return unwrapData(ctx.switchToHttp().getRequest<Request>().body);
});

/** A raw JSON object body (`/auth/local`, `/users/edit`, `/proxy`), or `{}`. */
export const RawBody = createParamDecorator((_: unknown, ctx: ExecutionContext): DataPayload => {
  const b: unknown = ctx.switchToHttp().getRequest<Request>().body;
  return isObj(b) ? b : {};
});
