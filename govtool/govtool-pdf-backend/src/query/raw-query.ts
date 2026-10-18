import { createParamDecorator, ExecutionContext } from '@nestjs/common';
import type { Request } from 'express';
import * as qs from 'qs';

/** Strapi's qs options (SPEC §2). */
export const QS_OPTIONS: qs.IParseOptions = {
  depth: 10,
  arrayLimit: 100,
  parameterLimit: 1000,
  allowDots: false,
};

/**
 * Parse a raw, possibly unencoded query string the way Strapi did: `+` is a
 * space, `%XX` is decoded, a malformed `%` stays literal (qs's decoder), `&`
 * separates pairs. A literal `#` starts a fragment and ends the query: clients
 * truncate there, and the backend does not try to repair it (§4.1).
 */
export function parseQueryString(raw: string): Record<string, unknown> {
  let s = raw.startsWith('?') ? raw.slice(1) : raw;
  const hash = s.indexOf('#');
  if (hash >= 0) s = s.slice(0, hash);
  if (s === '') return {};
  return qs.parse(s, QS_OPTIONS);
}

/** The query object of a URL (path + query). */
export function parseUrlQuery(url: string): Record<string, unknown> {
  const i = url.indexOf('?');
  return i < 0 ? {} : parseQueryString(url.slice(i + 1));
}

/**
 * The raw query object, parsed from `req.originalUrl` (never from Express's
 * parser). Pass it to parseQuery with the endpoint's allowlist.
 */
export const RawQuery = createParamDecorator((_: unknown, ctx: ExecutionContext) => {
  const req = ctx.switchToHttp().getRequest<Request>();
  return parseUrlQuery(req.originalUrl ?? req.url);
});
