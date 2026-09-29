// Destination rules of SPEC §9.2 (SSRF): which IP addresses the safe fetcher
// may connect to, and how a client URL is normalised before resolution.

import { isIP } from 'node:net';

type Cidr = [bytes: number[], prefix: number];

function v4(s: string): number[] | null {
  const parts = s.split('.');
  if (parts.length !== 4) return null;
  const out: number[] = [];
  for (const p of parts) {
    if (!/^\d{1,3}$/.test(p)) return null;
    const n = Number(p);
    if (n > 255) return null;
    out.push(n);
  }
  return out;
}

/** 16 bytes of an IPv6 literal (zone id and brackets stripped), or null. */
export function parseIPv6(input: string): number[] | null {
  let s = input.replace(/^\[|\]$/g, '');
  const zone = s.indexOf('%');
  if (zone >= 0) s = s.slice(0, zone);
  if (isIP(s) !== 6) return null;
  let tail: number[] = [];
  const lastColon = s.lastIndexOf(':');
  const last = s.slice(lastColon + 1);
  if (last.includes('.')) {
    const b = v4(last);
    if (!b) return null;
    tail = [(b[0] << 8) | b[1], (b[2] << 8) | b[3]];
    s = s.slice(0, lastColon + 1) + '0:0';
  }
  const [head, rest] = s.includes('::') ? s.split('::') : [s, null];
  const h = head === '' ? [] : head.split(':');
  const r = rest === null || rest === '' ? [] : rest.split(':');
  const fill = rest === null ? 0 : 8 - h.length - r.length;
  const groups = [...h, ...Array<string>(fill).fill('0'), ...r].map((g) => parseInt(g, 16));
  if (groups.length !== 8 || groups.some((g) => Number.isNaN(g))) return null;
  if (tail.length) groups.splice(6, 2, ...tail);
  return groups.flatMap((g) => [g >> 8, g & 0xff]);
}

function inCidr(bytes: number[], [net, prefix]: Cidr): boolean {
  for (let i = 0; i < prefix; i++) {
    const byte = i >> 3;
    const bit = 7 - (i & 7);
    if (((bytes[byte] >> bit) & 1) !== ((net[byte] >> bit) & 1)) return false;
  }
  return true;
}

const c4 = (s: string, prefix: number): Cidr => [v4(s)!, prefix];
const c6 = (s: string, prefix: number): Cidr => [parseIPv6(s)!, prefix];

const BLOCKED_V4: Cidr[] = [
  c4('0.0.0.0', 8), // "this network", unspecified
  c4('10.0.0.0', 8), // private
  c4('100.64.0.0', 10), // CGNAT
  c4('127.0.0.0', 8), // loopback
  c4('169.254.0.0', 16), // link-local (cloud metadata)
  c4('172.16.0.0', 12), // private
  c4('192.0.0.0', 24), // IETF protocol assignments
  c4('192.0.2.0', 24), // TEST-NET-1
  c4('192.88.99.0', 24), // 6to4 relay anycast
  c4('192.168.0.0', 16), // private
  c4('198.18.0.0', 15), // benchmarking
  c4('198.51.100.0', 24), // TEST-NET-2
  c4('203.0.113.0', 24), // TEST-NET-3
  c4('224.0.0.0', 4), // multicast
  c4('240.0.0.0', 4), // reserved, including 255.255.255.255 broadcast
];

const BLOCKED_V6: Cidr[] = [
  c6('::', 128), // unspecified
  c6('::1', 128), // loopback
  c6('100::', 64), // discard-only
  c6('2001:db8::', 32), // documentation
  c6('3fff::', 20), // documentation
  c6('fc00::', 7), // unique-local
  c6('fe80::', 10), // link-local
  c6('fec0::', 10), // site-local (deprecated)
  c6('ff00::', 8), // multicast
];

/** The IPv4 address an IPv6 address embeds (mapped, compatible, NAT64, 6to4). */
function embeddedV4(b: number[]): number[] | null {
  const zeros = (from: number, to: number) => b.slice(from, to).every((x) => x === 0);
  if (zeros(0, 10) && b[10] === 0xff && b[11] === 0xff) return b.slice(12); // ::ffff:a.b.c.d
  if (zeros(0, 12)) return b.slice(12); // ::a.b.c.d (compatible; :: and ::1 are caught above)
  if (b[0] === 0x00 && b[1] === 0x64 && b[2] === 0xff && b[3] === 0x9b && zeros(4, 12)) return b.slice(12); // 64:ff9b::/96
  if (b[0] === 0x20 && b[1] === 0x02) return b.slice(2, 6); // 2002::/16 6to4
  return null;
}

/** True when the fetcher must not connect to `address` (an IP literal). */
export function isBlockedAddress(address: string): boolean {
  const a4 = v4(address);
  if (a4) return BLOCKED_V4.some((c) => inCidr(a4, c));
  const a6 = parseIPv6(address);
  if (!a6) return true; // not an IP at all: never connect
  if (BLOCKED_V6.some((c) => inCidr(a6, c))) return true;
  const e = embeddedV4(a6);
  return e !== null && BLOCKED_V4.some((c) => inCidr(e, c));
}

/**
 * `ipfs://<cid>[/path]` to `<gateway>/<cid>[/path]`; other strings as given.
 * Returns null for an `ipfs://` URL with no CID.
 */
export function rewriteIpfs(url: string, gateway: string): string | null {
  if (!/^ipfs:\/\//i.test(url)) return url;
  const rest = url.slice('ipfs://'.length).replace(/^\/+/, '');
  if (rest === '') return null;
  return `${gateway.replace(/\/+$/, '')}/${rest}`;
}

/** An http(s) URL with no userinfo, or null. */
export function parseFetchableUrl(raw: string): URL | null {
  let u: URL;
  try {
    u = new URL(raw);
  } catch {
    return null;
  }
  if (u.protocol !== 'http:' && u.protocol !== 'https:') return null;
  if (u.username !== '' || u.password !== '') return null;
  if (u.hostname === '') return null;
  return u;
}

/**
 * §9.1 path matcher: the raw (still encoded) path after `/proxy/govtool/`
 * must be free of traversal and encoded separators, and then match an
 * allowlisted path exactly.
 */
export function matchGovtoolPath(rawPath: string, allowed: readonly string[]): string | null {
  if (rawPath === '') return null;
  if (/\.\.|\\|\/\/|%2f|%2e|%5c/i.test(rawPath)) return null;
  let decoded: string;
  try {
    decoded = decodeURIComponent(rawPath);
  } catch {
    return null;
  }
  if (/\.\.|\\|\/\/|%/.test(decoded)) return null;
  return allowed.includes(decoded) ? decoded : null;
}
