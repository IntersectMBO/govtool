import * as ipaddr from "ipaddr.js";

/**
 * The service connects only to globally routable public unicast addresses.
 * This is an allowlist: an address is public only when ipaddr.js classifies it
 * as `unicast` AND it falls in none of the ranges below. The explicit table
 * mirrors the decision record so a library change cannot silently widen it.
 */
const DENIED_CIDRS: [string, string][] = [
  ["0.0.0.0/8", "unspecified"],
  ["10.0.0.0/8", "private"],
  ["100.64.0.0/10", "carrierGradeNat"],
  ["127.0.0.0/8", "loopback"],
  ["169.254.0.0/16", "linkLocal"],
  ["172.16.0.0/12", "private"],
  ["192.0.0.0/24", "reserved"],
  ["192.0.2.0/24", "reserved"],
  ["192.88.99.0/24", "reserved"],
  ["192.168.0.0/16", "private"],
  ["198.18.0.0/15", "reserved"],
  ["198.51.100.0/24", "reserved"],
  ["203.0.113.0/24", "reserved"],
  ["224.0.0.0/4", "multicast"],
  ["255.255.255.255/32", "broadcast"],
  ["240.0.0.0/4", "reserved"],
  ["::/128", "unspecified"],
  ["::1/128", "loopback"],
  ["::ffff:0:0/96", "ipv4Mapped"],
  ["64:ff9b::/96", "rfc6052"],
  ["100::/64", "reserved"],
  ["2001:db8::/32", "reserved"],
  ["2002::/16", "6to4"],
  ["fc00::/7", "uniqueLocal"],
  ["fe80::/10", "linkLocal"],
  ["ff00::/8", "multicast"],
];

const denied = DENIED_CIDRS.map(([cidr, range]) => ({ cidr: ipaddr.parseCIDR(cidr), range }));

let testAllowed = new Set<string>();

/**
 * Test-only escape hatch so the suite can reach a mock server on loopback.
 * Deliberately code, not configuration: nothing in a deployment can set it.
 */
export const allowAddressesForTesting = (addresses: string[]) => {
  testAllowed = new Set(addresses);
};

/**
 * Local testing only (D137, amending D122): METADATA_ALLOW_PRIVATE_ADDRESSES=true
 * treats every address as connectable, so a local test bucket on loopback or a
 * private network can be fetched. Read on each call so a test can toggle it.
 * Never set this in a deployment.
 */
export const privateAddressesAllowed = (): boolean =>
  process.env.METADATA_ALLOW_PRIVATE_ADDRESSES?.trim().toLowerCase() === "true";

export type AddressVerdict = { public: true } | { public: false; range: string };

export const classifyAddress = (address: string): AddressVerdict => {
  if (testAllowed.has(address) || privateAddressesAllowed()) return { public: true };
  let parsed: ipaddr.IPv4 | ipaddr.IPv6;
  try {
    parsed = ipaddr.parse(address);
  } catch {
    return { public: false, range: "invalid" };
  }
  // An IPv4-mapped IPv6 address is judged as the IPv4 address it carries.
  if (parsed.kind() === "ipv6" && (parsed as ipaddr.IPv6).isIPv4MappedAddress()) {
    return classifyAddress((parsed as ipaddr.IPv6).toIPv4Address().toString());
  }
  // NAT64 (64:ff9b::/96) and 6to4 (2002::/16) also embed an IPv4 address.
  // They are refused outright rather than unwrapped: a server has no need to
  // reach a metadata host through a translation prefix.
  for (const { cidr, range } of denied) {
    if (parsed.kind() === cidr[0].kind() && parsed.match(cidr)) return { public: false, range };
  }
  const range = parsed.range();
  return range === "unicast" ? { public: true } : { public: false, range };
};
