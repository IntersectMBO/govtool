import { GATEWAY_BLACKLIST_MS, IPFS_GATEWAYS } from "../config";

/** An IPFS or IPNS address, independent of whichever gateway a url named. */
export interface IpfsAddress {
  namespace: "ipfs" | "ipns";
  /** A CID for ipfs, a key or DNS name for ipns. */
  id: string;
  /** Anything after the id: a path inside the content, and any query. */
  rest: string;
}

const CID_V0 = /^Qm[1-9A-HJ-NP-Za-km-z]{44}$/;
const CID_V1 = /^b[a-z2-7]{50,}$/i;
const IPNS_NAME = /^[A-Za-z0-9][A-Za-z0-9.-]*$/;

const isCid = (value: string) => CID_V0.test(value) || CID_V1.test(value);

const valid = (namespace: "ipfs" | "ipns", id: string) =>
  namespace === "ipfs" ? isCid(id) : IPNS_NAME.test(id);

/**
 * Recognise an IPFS anchor in any of its forms: `ipfs://<cid>`, `ipns://<name>`,
 * a path gateway url `https://<any host>/ipfs/<cid>`, or a subdomain gateway url
 * `https://<cid>.ipfs.<any host>`. The gateway a url names is ignored: the
 * service fetches the content through its own gateways.
 */
export const parseIpfsUrl = (url: string): IpfsAddress | undefined => {
  let u: URL;
  try {
    u = new URL(url);
  } catch {
    return undefined;
  }
  const rest = (path: string) => `${path}${u.search}`;

  if (u.protocol === "ipfs:" || u.protocol === "ipns:") {
    const namespace = u.protocol === "ipfs:" ? "ipfs" : "ipns";
    // WHATWG URL puts the id in the host for ipfs://<id>/path.
    const id = u.host || u.pathname.replace(/^\/+/, "").split("/")[0];
    const path = u.host ? u.pathname : u.pathname.replace(/^\/+[^/]+/, "");
    return id && valid(namespace, id) ? { namespace, id, rest: rest(path === "/" ? "" : path) } : undefined;
  }
  if (u.protocol !== "http:" && u.protocol !== "https:") return undefined;

  const sub = u.hostname.match(/^([^.]+)\.(ipfs|ipns)\.[^.]+/i);
  if (sub) {
    const namespace = sub[2].toLowerCase() as "ipfs" | "ipns";
    if (valid(namespace, sub[1])) {
      return { namespace, id: sub[1], rest: rest(u.pathname === "/" ? "" : u.pathname) };
    }
  }
  const path = u.pathname.match(/^\/(ipfs|ipns)\/([^/]+)(\/.*)?$/);
  if (path) {
    const namespace = path[1] as "ipfs" | "ipns";
    if (valid(namespace, path[2])) return { namespace, id: path[2], rest: rest(path[3] ?? "") };
  }
  return undefined;
};

export const gatewayUrl = (gateway: string, address: IpfsAddress) =>
  `${gateway.replace(/\/+$/, "")}/${address.namespace}/${address.id}${address.rest}`;

/* -- which gateways to try, and in what order ------------------------------ */

let gatewaysOverride: string[] | undefined;

/** Test-only: replace the configured gateway list. Code, not configuration. */
export const setIpfsGatewaysForTesting = (gateways: string[] | undefined) => {
  gatewaysOverride = gateways;
  blacklist.clear();
};

const blacklist = new Map<string, number>();

/** Skip `gateway` for GATEWAY_BLACKLIST_MS: it said it is unavailable. */
export const blacklistGateway = (gateway: string) => {
  blacklist.set(gateway, Date.now() + GATEWAY_BLACKLIST_MS);
};

const isBlacklisted = (gateway: string) => {
  const until = blacklist.get(gateway);
  if (until === undefined) return false;
  if (until > Date.now()) return true;
  blacklist.delete(gateway);
  return false;
};

/** The primary gateway from the environment, when it is a valid http(s) url. */
const primaryGateway = (): string | undefined => {
  const value = process.env.IPFS_PRIMARY_GATEWAY?.trim().replace(/\/+$/, "");
  if (!value) return undefined;
  try {
    const u = new URL(value);
    return u.protocol === "http:" || u.protocol === "https:" ? value : undefined;
  } catch {
    return undefined;
  }
};

const shuffle = <T>(items: T[]): T[] => {
  const out = [...items];
  for (let i = out.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [out[i], out[j]] = [out[j], out[i]];
  }
  return out;
};

/**
 * The gateways to try for one fetch, in order, without blacklisted ones. With
 * a primary: the primary, then the list in its configured order. Without one:
 * the list in a random order, so load spreads across gateways.
 */
export const gatewayOrder = (): { order: string[]; skipped: string[] } => {
  const configured = (gatewaysOverride ?? IPFS_GATEWAYS).map((g) => g.replace(/\/+$/, ""));
  const primary = primaryGateway();
  const all = primary ? [primary, ...configured.filter((g) => g !== primary)] : shuffle(configured);
  return {
    order: all.filter((g) => !isBlacklisted(g)),
    skipped: all.filter((g) => isBlacklisted(g)),
  };
};
