import { isIPv4, isIPv6 } from 'node:net';

/**
 * IP addresses leave the backend with their middle hidden (D171): an IPv4
 * address keeps its first and last octet, `203.xx.xx.45`, and an IPv6 address
 * its first and last group, `2606:xx:xx:84e5`. That is enough to tell
 * addresses apart in a fetch report without publishing them.
 */
const HIDDEN = 'xx';

const IPV4 = /(?<![\w.])\d{1,3}(?:\.\d{1,3}){3}(?!\w|\.\d)/g;

/**
 * A run of hex digits, dots and at least two colons. Node writes an IPv6
 * peer as `address:port`, unbracketed, so a candidate that is not an address
 * is retried without its last group.
 */
const IPV6_CANDIDATE = /(?<![\w:.])[0-9a-f.]*:[0-9a-f.]*:[0-9a-f.:]*(?![\w])/gi;

const redactIpv4 = (address: string): string => {
  const octets = address.split('.');
  return `${octets[0]}.${HIDDEN}.${HIDDEN}.${octets[3]}`;
};

const redactIpv6 = (address: string): string => {
  // An IPv4-mapped or -compatible address hides its IPv4 part as IPv4.
  const embedded = address.match(/^(.*:)(\d{1,3}(?:\.\d{1,3}){3})$/);
  if (embedded && isIPv4(embedded[2])) {
    return `${embedded[1]}${redactIpv4(embedded[2])}`;
  }
  let first: string;
  let last: string;
  if (address.includes('::')) {
    // Either side of `::` may be empty, standing for zero groups.
    const [head, tail] = address.split('::');
    first = head ? head.split(':')[0] : '0';
    last = tail ? tail.split(':').at(-1)! : '0';
  } else {
    const groups = address.split(':');
    first = groups[0];
    last = groups.at(-1)!;
  }
  return `${first}:${HIDDEN}:${HIDDEN}:${last}`;
};

/** One address, as an `address` field holds it. Anything else is unchanged. */
export const redactIp = (address: string): string => {
  if (isIPv4(address)) return redactIpv4(address);
  if (isIPv6(address)) return redactIpv6(address);
  return address;
};

const redactIpv6Candidate = (candidate: string): string => {
  // Sentence punctuation is not part of the address.
  const trimmed = candidate.replace(/[.:]+$/, '');
  const rest = candidate.slice(trimmed.length);
  if (isIPv6(trimmed)) return `${redactIpv6(trimmed)}${rest}`;
  const port = trimmed.match(/^(.*):(\d{1,5})$/);
  if (port && isIPv6(port[1])) {
    return `${redactIpv6(port[1])}:${port[2]}${rest}`;
  }
  return candidate;
};

/** Every address found in free text, such as an error message. */
export const redactIpsInText = (text: string): string =>
  text
    .replace(IPV6_CANDIDATE, redactIpv6Candidate)
    .replace(IPV4, (match) => (isIPv4(match) ? redactIpv4(match) : match));

/**
 * Every string in `value`, recursively, except under the keys in `keep`.
 * Arrays and plain objects are copied; the input is not modified.
 */
export const redactIpsDeep = <T>(
  value: T,
  keep: ReadonlySet<string> = new Set(),
): T => {
  if (typeof value === 'string') return redactIpsInText(value) as T;
  if (Array.isArray(value)) {
    return value.map((item: unknown) => redactIpsDeep(item, keep)) as T;
  }
  if (value !== null && typeof value === 'object') {
    return Object.fromEntries(
      Object.entries(value).map(([key, item]) => [
        key,
        keep.has(key) ? item : redactIpsDeep(item, keep),
      ]),
    ) as T;
  }
  return value;
};
