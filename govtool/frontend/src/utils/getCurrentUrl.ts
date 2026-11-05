import {
  GOVERNANCE_ACTION_TYPE_PARAM,
  LEGACY_PATH_SEGMENTS,
} from "@/consts/paths";

import { getGovActionCIP129Id } from "./getGovActionId";

type Url = { pathname: string; search: string; hash: string };

const TX_HASH = /^[0-9a-fA-F]{64}$/;

// The router matches paths in any case, so old links did too (/edit_dRep).
const renameSegment = (segment: string) => {
  const key = segment.toLowerCase();
  if (!Object.prototype.hasOwnProperty.call(LEGACY_PATH_SEGMENTS, key)) {
    return [segment];
  }
  return LEGACY_PATH_SEGMENTS[key].split("/");
};

const isSegment = (segment: string | undefined, name: string) =>
  segment?.toLowerCase() === name;

/**
 * The current URL for a link from before GovTool's URLs named resources, or
 * null when the URL is already current. It drops the /connected prefix,
 * renames underscore segments (/drep_directory → /dreps), turns
 * /actions/category/:type into /actions?type=:type, and names an action by
 * its CIP-129 id instead of a transaction hash with the index in the hash
 * (/actions/<txHash>#0 → /actions/gov_action1…).
 */
export const getCurrentUrl = ({ pathname, search, hash }: Url): Url | null => {
  let [, ...segments] = pathname.split("/");
  let nextSearch = search;
  let nextHash = hash;

  if (isSegment(segments[0], "connected")) segments = segments.slice(1);
  segments = segments.flatMap(renameSegment);

  if (segments[0] === "proposals" && isSegment(segments[1], "propose")) {
    segments = ["proposals", "new", ...segments.slice(2)];
  }

  if (
    segments[0] === "actions" &&
    isSegment(segments[1], "category") &&
    segments[2]
  ) {
    const [, , type, ...rest] = segments;
    if (rest.length) {
      segments = ["actions", ...rest];
    } else {
      const params = new URLSearchParams(search);
      params.set(GOVERNANCE_ACTION_TYPE_PARAM, decodeURIComponent(type));
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
