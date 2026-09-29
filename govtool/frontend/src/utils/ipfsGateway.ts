import { env } from "@/config/env";

/** What GovTool has always used when VITE_IPFS_GATEWAY is unset. */
export const DEFAULT_IPFS_GATEWAY = "https://ipfs.io/ipfs";

/** The IPFS path gateway base url, `.../ipfs`, with no trailing slash. */
export const getIpfsGateway = (): string =>
  String(env.VITE_IPFS_GATEWAY || DEFAULT_IPFS_GATEWAY).replace(/\/+$/, "");

/** `<gateway>/<cid>[/path]` for a CID or the part of an ipfs:// url after the scheme. */
export const ipfsGatewayUrl = (cidPath: string): string =>
  `${getIpfsGateway()}/${cidPath.replace(/^\/+/, "")}`;
