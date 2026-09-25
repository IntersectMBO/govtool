/**
 * Service limits. These are code, not environment: changing one is a reviewed
 * change, not a deployment toggle.
 */

/** Largest document accepted, and the most a fetch report keeps (2 MB). */
export const FETCH_LIMIT_BYTES = 2 * 1024 * 1024;

/** How long a failure for a (url, hash) pair is replayed without refetching. */
export const ERROR_TTL_MS = 60 * 1000;

/** At most one real refetch per (url, hash) per window, whoever asks. */
export const REFRESH_WINDOW_MS = 60 * 1000;

/** Redirects followed before giving up. */
export const REDIRECT_LIMIT = 10;

/** Idle socket timeout per stage for http(s) urls. */
export const HTTP_TIMEOUT_MS = 40 * 1000;

/**
 * Public IPFS gateways, used for every IPFS anchor. Without a primary gateway
 * (IPFS_PRIMARY_GATEWAY) each request tries them in a random order; with one,
 * the primary goes first and these follow in this order. The public ipfs.io,
 * dweb.link, w3s.link and nftstorage.link gateways were retired on 2026-09-21.
 */
export const IPFS_GATEWAYS = [
  "https://ipfs.blockfrost.dev",
  "https://c-ipfs-gw.nmkr.io",
  "https://ipfs.filebase.io",
  "https://gateway.pinata.cloud",
];

/** Idle socket timeout per stage for each IPFS gateway tried. */
export const IPFS_TIMEOUT_MS = 15 * 1000;

/** A gateway that answers 429 or 503 is skipped for this long. */
export const GATEWAY_BLACKLIST_MS = 3 * 60 * 1000;
