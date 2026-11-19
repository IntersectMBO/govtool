import {
  GOVERNANCE_ACTION_TYPE_PARAM,
  LEGACY_CHILD_SEGMENTS,
  LEGACY_PATH_SEGMENTS,
} from "@/consts/paths";

import { getGovActionCIP129Id } from "./getGovActionId";

type Url = { pathname: string; search: string; hash: string };

const TX_HASH = /^[0-9a-fA-F]{64}$/;

// The router matches paths in any case, so old links did too (/edit_dRep).
const lookup = (names: Record<string, string>, segment = "") => {
  const key = segment.toLowerCase();
  return Object.prototype.hasOwnProperty.call(names, key)
    ? names[key]
    : undefined;
};

const decode = (segment: string) => {
  try {
    return decodeURIComponent(segment);
  } catch {
    return segment;
  }
};

/**
 * The current URL for a link from before GovTool's URLs named resources, or
 * null when the URL is already current. It drops the /connected prefix,
 * renames the old route names (/drep_directory → /dreps), turns
 * /actions/category/:type into /actions?type=:type, and names an action by
 * its CIP-129 id instead of a transaction hash with the index in the hash
 * (/actions/<txHash>#0 → /actions/gov_action1…). Only route names are
 * renamed, never an id or slug that happens to match one.
 */
export const getCurrentUrl = ({ pathname, search, hash }: Url): Url | null => {
  let [, ...segments] = pathname.split("/");
  let nextSearch = search;
  let nextHash = hash;

  if (segments[0]?.toLowerCase() === "connected") segments = segments.slice(1);

  const first = lookup(LEGACY_PATH_SEGMENTS, segments[0]);
  if (first) segments = [...first.split("/"), ...segments.slice(1)];

  const children = LEGACY_CHILD_SEGMENTS[segments[0]];
  const child = children && lookup(children, segments[1]);
  if (child) segments = [segments[0], child, ...segments.slice(2)];

  if (
    segments[0] === "actions" &&
    segments[1]?.toLowerCase() === "category" &&
    segments[2]
  ) {
    const [, , type, ...rest] = segments;
    if (rest.length) {
      segments = ["actions", ...rest];
    } else {
      const params = new URLSearchParams(search);
      params.set(GOVERNANCE_ACTION_TYPE_PARAM, decode(type));
      segments = ["actions"];
      nextSearch = `?${params}`;
    }
  }

  const idAt = segments[0] === "actions" && segments[1] === "history" ? 2 : 1;
  if (
    segments[0] === "actions" &&
    segments.length === idAt + 1 &&
    TX_HASH.test(segments[idAt])
  ) {
    const index = parseInt(hash.slice(1), 10);
    segments[idAt] = getGovActionCIP129Id(
      segments[idAt].toLowerCase(),
      Number.isNaN(index) ? 0 : index,
    );
    nextHash = "";
  }

  const nextPathname = `/${segments.join("/")}`;
  if (nextPathname === pathname && nextSearch === search && nextHash === hash) {
    return null;
  }
  return { pathname: nextPathname, search: nextSearch, hash: nextHash };
};
