// SPEC §9. Bodies are Strapi's proxy shape, not the §3.6 envelope, so every
// failure is a RawHttpError except the §9.1 404, which is N.

import { Inject, Injectable } from '@nestjs/common';
import { RawHttpError, notFound } from '../common/errors';
import { APP_CONFIG } from '../config/config.module';
import type { AppConfig } from '../config/config';
import { matchGovtoolPath, rewriteIpfs } from './address-guard';
import { FetchError, FetchOptions, FetchResult, decodeBody, safeGet } from './fetcher';

const USER_AGENT = 'govtool-pdf-proxy';

const raw = (status: number, error: string, details: unknown = null) =>
  new RawHttpError(status, { error, details });

export interface ProxyResponse {
  status: number;
  data: unknown;
}

@Injectable()
export class ProxyService {
  constructor(@Inject(APP_CONFIG) private readonly config: AppConfig) {}

  private async run(url: string, opts: FetchOptions): Promise<ProxyResponse> {
    let r: FetchResult;
    try {
      r = await safeGet(url, opts);
    } catch (e) {
      if (e instanceof FetchError && e.kind === 'invalid-url') throw raw(400, 'Invalid URL');
      if (e instanceof FetchError && e.kind === 'blocked') throw raw(400, 'Destination not allowed');
      throw raw(502, 'Upstream request failed');
    }
    const data = r.body.length === 0 ? null : decodeBody(r);
    if (r.status < 200 || r.status > 299) {
      throw raw(r.status, `Request failed with status code ${r.status}`, data);
    }
    return { status: r.status, data };
  }

  /**
   * §9.1: `rawPath` is the still-encoded path after `/api/proxy/govtool/`,
   * `rawQuery` the query string without `?`.
   */
  govtool(rawPath: string, rawQuery: string): Promise<ProxyResponse> {
    const path = matchGovtoolPath(rawPath, this.config.govtoolProxyAllowedPaths);
    if (path === null) throw notFound();
    const base = this.config.govtoolApiBaseUrl;
    if (!base) throw raw(503, 'GOVTOOL_API_BASE_URL is not configured');
    // Re-serialized from the parsed pairs; nothing of the client's raw string survives.
    const query = new URLSearchParams(rawQuery).toString();
    const url = `${base}/${path}${query ? `?${query}` : ''}`;
    return this.run(url, {
      timeoutMs: this.config.proxyTimeoutMs,
      maxBytes: this.config.proxyMaxBytes,
      maxRedirects: 0,
      headers: { Accept: 'application/json', 'User-Agent': USER_AGENT },
      // The base URL is operator configuration, typically a private address.
      checkAddress: false,
    });
  }

  /** §9.2: the safe public-URL fetcher. */
  fetch(body: Record<string, unknown>): Promise<ProxyResponse> {
    const method = body.method;
    if (
      method !== undefined &&
      method !== null &&
      (typeof method !== 'string' || method.toUpperCase() !== 'GET')
    ) {
      throw raw(400, 'Only GET is supported');
    }
    if (typeof body.url !== 'string' || body.url.trim() === '') throw raw(400, 'Invalid URL');
    const url = rewriteIpfs(body.url.trim(), this.config.ipfsGatewayUrl);
    if (url === null) throw raw(400, 'Invalid URL');
    return this.run(url, {
      timeoutMs: this.config.proxyTimeoutMs,
      maxBytes: this.config.proxyMaxBytes,
      maxRedirects: 3,
      headers: { Accept: '*/*', 'User-Agent': USER_AGENT },
      checkAddress: !this.config.allowPrivateUrls,
      // Empty unless PDF_ALLOW_PRIVATE_URLS=true (enforced by loadConfig).
      connectTo: this.config.proxyHostRewrites,
    });
  }
}
