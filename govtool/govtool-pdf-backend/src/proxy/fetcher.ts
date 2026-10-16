// Outbound GET for both proxies (SPEC §9). Connects to an address chosen
// before the request (pinned through `lookup`, so no second DNS resolution
// can swap it), follows at most `maxRedirects` redirects with every hop
// re-validated, and enforces the timeout and the body limit while streaming.

import { lookup as dnsLookup } from 'node:dns/promises';
import * as http from 'node:http';
import * as https from 'node:https';
import { isIP } from 'node:net';
import { isBlockedAddress, parseFetchableUrl } from './address-guard';

export type FetchFailure =
  /** The URL (or a redirect target) is not an http(s) URL without userinfo. */
  | 'invalid-url'
  /** The destination resolves to an address the guard blocks. */
  | 'blocked'
  /** Network error, timeout, oversize, too many redirects. */
  | 'upstream';

export class FetchError extends Error {
  constructor(public readonly kind: FetchFailure) {
    super(kind);
    this.name = 'FetchError';
  }
}

export interface FetchOptions {
  timeoutMs: number;
  maxBytes: number;
  maxRedirects: number;
  headers: Record<string, string>;
  /**
   * Refuse destinations whose address the guard blocks. Off for the
   * operator-configured GovTool base URL and under PDF_ALLOW_PRIVATE_URLS.
   */
  checkAddress: boolean;
  /**
   * `hostname:port` -> where to connect instead (PDF_PROXY_HOST_REWRITES, tests
   * only). The URL and its Host header stay as they are; only the TCP target
   * changes, on every hop.
   */
  connectTo?: ReadonlyMap<string, HostPort>;
  /** Injected for tests; defaults to the §9.2 guard. */
  isBlocked?: (address: string) => boolean;
  /** Injected for tests; defaults to dns.lookup(all). */
  resolve?: (host: string) => Promise<Array<{ address: string; family: number }>>;
}

export interface HostPort {
  host: string;
  port: number;
}

export interface FetchResult {
  status: number;
  contentType: string | null;
  body: Buffer;
}

async function resolveAll(host: string): Promise<Array<{ address: string; family: number }>> {
  return dnsLookup(host, { all: true, verbatim: true });
}

const effectivePort = (url: URL) => Number(url.port || (url.protocol === 'https:' ? 443 : 80));

/** The PDF_PROXY_HOST_REWRITES target for `url`, if any. */
function connectTarget(url: URL, opts: FetchOptions): HostPort {
  return (
    opts.connectTo?.get(`${url.hostname.toLowerCase()}:${effectivePort(url)}`) ?? {
      host: url.hostname,
      port: effectivePort(url),
    }
  );
}

/** Resolve `target`'s host and pick the address to pin; every record must pass. */
async function pickAddress(
  target: HostPort,
  opts: FetchOptions,
): Promise<{ address: string; family: number }> {
  const host = target.host.replace(/^\[|\]$/g, '');
  const literal = isIP(host);
  let records: Array<{ address: string; family: number }>;
  if (literal) {
    records = [{ address: host, family: literal }];
  } else {
    try {
      records = await (opts.resolve ?? resolveAll)(host);
    } catch {
      throw new FetchError('upstream');
    }
  }
  if (records.length === 0) throw new FetchError('upstream');
  if (opts.checkAddress) {
    const blocked = opts.isBlocked ?? isBlockedAddress;
    if (records.some((r) => blocked(r.address))) throw new FetchError('blocked');
  }
  return records[0];
}

interface HopResult {
  status: number;
  location: string | null;
  contentType: string | null;
  body: Buffer;
}

function requestOnce(
  url: URL,
  target: HostPort,
  pinned: { address: string; family: number },
  opts: FetchOptions,
  signal: AbortSignal,
): Promise<HopResult> {
  return new Promise((resolve, reject) => {
    const mod = url.protocol === 'https:' ? https : http;
    const lookup = (
      _host: string,
      o: { all?: boolean },
      cb: (
        err: Error | null,
        address: string | Array<{ address: string; family: number }>,
        family?: number,
      ) => void,
    ) => {
      if (o && o.all) cb(null, [pinned]);
      else cb(null, pinned.address, pinned.family);
    };
    const req = mod.request(
      url,
      {
        method: 'GET',
        // The connect target. Node skips `lookup` for an IP literal, so a
        // rewritten literal host must be replaced here, not only pinned.
        hostname: target.host.replace(/^\[|\]$/g, ''),
        port: target.port,
        ...(url.protocol === 'https:' && !isIP(url.hostname.replace(/^\[|\]$/g, ''))
          ? { servername: url.hostname }
          : {}),
        // Stated explicitly so a rewritten port never leaks into it.
        headers: { ...opts.headers, Host: url.host },
        agent: false,
        lookup: lookup,
        signal,
      },
      (res) => {
        const status = res.statusCode ?? 0;
        const location = typeof res.headers.location === 'string' ? res.headers.location : null;
        const contentType =
          typeof res.headers['content-type'] === 'string' ? res.headers['content-type'] : null;
        if (status >= 300 && status < 400 && location !== null) {
          res.resume();
          resolve({ status, location, contentType, body: Buffer.alloc(0) });
          return;
        }
        const declared = Number(res.headers['content-length']);
        if (Number.isFinite(declared) && declared > opts.maxBytes) {
          res.destroy();
          reject(new FetchError('upstream'));
          return;
        }
        const chunks: Buffer[] = [];
        let size = 0;
        res.on('data', (chunk: Buffer) => {
          size += chunk.length;
          if (size > opts.maxBytes) {
            res.destroy();
            reject(new FetchError('upstream'));
            return;
          }
          chunks.push(chunk);
        });
        res.on('end', () => resolve({ status, location: null, contentType, body: Buffer.concat(chunks) }));
        res.on('error', () => reject(new FetchError('upstream')));
        res.on('aborted', () => reject(new FetchError('upstream')));
      },
    );
    req.on('error', () => reject(new FetchError('upstream')));
    req.end();
  });
}

/** GET `raw` under the rules in `opts`. Throws FetchError. */
export async function safeGet(raw: string, opts: FetchOptions): Promise<FetchResult> {
  const signal = AbortSignal.timeout(opts.timeoutMs);
  let url = parseFetchableUrl(raw);
  if (!url) throw new FetchError('invalid-url');
  for (let hop = 0; ; hop++) {
    const target = connectTarget(url, opts);
    const pinned = await pickAddress(target, opts);
    if (signal.aborted) throw new FetchError('upstream');
    const res = await requestOnce(url, target, pinned, opts, signal);
    if (res.location === null) return { status: res.status, contentType: res.contentType, body: res.body };
    if (hop >= opts.maxRedirects) throw new FetchError('upstream');
    let next: string;
    try {
      next = new URL(res.location, url).toString();
    } catch {
      throw new FetchError('invalid-url');
    }
    url = parseFetchableUrl(next);
    if (!url) throw new FetchError('invalid-url');
  }
}

/** JSON when the content type says so (and it parses), else the UTF-8 text. */
export function decodeBody(r: FetchResult): unknown {
  const text = r.body.toString('utf8');
  if (r.contentType && /^application\/(?:[\w.+-]*\+)?json\b/i.test(r.contentType.trim())) {
    try {
      return JSON.parse(text) as unknown;
    } catch {
      return text;
    }
  }
  return text;
}
