// Assertions on the Strapi v4 envelope (§3.1) and error body (§3.6).

import type { Response } from 'supertest';

type ErrorName =
  | 'BadRequestError'
  | 'ValidationError'
  | 'ApplicationError'
  | 'UnauthorizedError'
  | 'ForbiddenError'
  | 'NotFoundError'
  | 'PayloadTooLargeError'
  | 'InternalServerError';

/** Exact error body: status, name, message and (default `{}`) details. */
export function expectError(
  res: Response,
  status: number,
  name: ErrorName,
  message: string,
  details: unknown = {},
) {
  expect({ status: res.status, body: res.body }).toEqual({
    status,
    body: { data: null, error: { status, name, message, details } },
  });
}

/** 400 BD: message `Bad Request`, the text in details. */
export const expectBadRequestDetails = (res: Response, text: string) =>
  expectError(res, 400, 'BadRequestError', 'Bad Request', text);
export const expectValidation = (res: Response, message: string) =>
  expectError(res, 400, 'ValidationError', message);
export const expectForbidden = (res: Response, message = 'Forbidden') =>
  expectError(res, 403, 'ForbiddenError', message);
export const expectUnauthorized = (res: Response, message = 'Missing or invalid credentials') =>
  expectError(res, 401, 'UnauthorizedError', message);
export const expectNotFound = (res: Response, message = 'Not Found') =>
  expectError(res, 404, 'NotFoundError', message);

/** `{id: int, attributes: {...}}` */
export function expectEntity(e: unknown): asserts e is { id: number; attributes: Record<string, unknown> } {
  expect(e).toEqual({ id: expect.any(Number), attributes: expect.any(Object) });
}

const ISO_MS = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/;

/** createdAt/updatedAt (and publishedAt when asked) as ISO 8601 with ms. */
export function expectTimestamps(attributes: Record<string, unknown>, publishedAt = false) {
  expect(attributes.createdAt).toMatch(ISO_MS);
  expect(attributes.updatedAt).toMatch(ISO_MS);
  if (publishedAt) expect(attributes.publishedAt).toMatch(ISO_MS);
  else expect(attributes).not.toHaveProperty('publishedAt');
}

/** 200 single envelope; returns data. */
export function expectSingle(res: Response) {
  expect(res.status).toBe(200);
  expect(Object.keys(res.body).sort()).toEqual(['data', 'meta']);
  expect(res.body.meta).toEqual({});
  if (res.body.data !== null) expectEntity(res.body.data);
  return res.body.data as { id: number; attributes: Record<string, unknown> } | null;
}

/**
 * 200 list envelope with page pagination; checks pageCount against total and
 * the optional expectations. Returns data.
 */
export function expectList(
  res: Response,
  expected: { page?: number; pageSize?: number; total?: number; length?: number } = {},
) {
  expect(res.status).toBe(200);
  expect(Object.keys(res.body).sort()).toEqual(['data', 'meta']);
  const p = res.body.meta.pagination;
  expect(p).toEqual({
    page: expected.page ?? expect.any(Number),
    pageSize: expected.pageSize ?? expect.any(Number),
    pageCount: expect.any(Number),
    total: expected.total ?? expect.any(Number),
  });
  expect(p.pageCount).toBe(p.total === 0 ? 0 : Math.ceil(p.total / p.pageSize));
  expect(Array.isArray(res.body.data)).toBe(true);
  for (const e of res.body.data) expectEntity(e);
  if (expected.length !== undefined) expect(res.body.data).toHaveLength(expected.length);
  return res.body.data as Array<{ id: number; attributes: Record<string, unknown> }>;
}

/** Recursively assert a JSON body never carries a key (e.g. hash, email). */
export function expectNoKeyDeep(body: unknown, key: string) {
  const walk = (v: unknown, path: string) => {
    if (Array.isArray(v)) v.forEach((x, i) => walk(x, `${path}[${i}]`));
    else if (v && typeof v === 'object') {
      for (const [k, x] of Object.entries(v)) {
        if (k === key) throw new Error(`Forbidden key "${key}" at ${path}.${k}`);
        walk(x, `${path}.${k}`);
      }
    }
  };
  walk(body, '$');
}
