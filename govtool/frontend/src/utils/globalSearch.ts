import {
  Address,
  BaseAddress,
  RewardAddress,
} from "@emurgo/cardano-serialization-lib-asmjs";

// Helpers for the header's global search.

export type SearchQueryKind = "keyword" | "address" | "handle";

export type ClassifiedQuery = {
  /** The query as typed, trimmed. */
  text: string;
  kind: SearchQueryKind;
  /** A handle to look up: a $ query, or any one-word query. */
  handle?: string;
  /** The stake address a pasted address belongs to. */
  stakeAddress?: string;
};

const ADDRESS_PREFIX = /^(addr|addr_test|stake|stake_test)1/i;
// ADA handles: 1–15 of a-z, 0-9, "-", "_" and ".".
const HANDLE = /^\$?([a-z0-9._-]{1,15})$/i;
// Ids that are legal handle text but never meant as one.
const ID_PREFIX = /^(gov_action|drep|cc_cold|cc_hot|pool)/i;

/** The bech32 stake address an address belongs to, if it has one. */
export const stakeAddressOf = (address: string) => {
  try {
    const parsed = Address.from_bech32(address);
    if (RewardAddress.from_address(parsed)) return parsed.to_bech32();
    const base = BaseAddress.from_address(parsed);
    if (!base) return undefined;
    return RewardAddress.new(parsed.network_id(), base.stake_cred())
      .to_address()
      .to_bech32();
  } catch {
    return undefined;
  }
};

/** What a query looks like: words, a pasted address or a handle. */
export const classifyQuery = (raw: string): ClassifiedQuery => {
  const text = raw.trim();
  if (ADDRESS_PREFIX.test(text) && !text.includes(" ")) {
    const stakeAddress = stakeAddressOf(text);
    return stakeAddress ? { text, kind: "address", stakeAddress } : { text, kind: "address" };
  }
  const handle = ID_PREFIX.test(text) ? undefined : HANDLE.exec(text)?.[1]?.toLowerCase();
  if (text.startsWith("$") && handle) return { text, kind: "handle", handle };
  return handle ? { text, kind: "keyword", handle } : { text, kind: "keyword" };
};

export type RecentSearch = { query: string; kind: SearchQueryKind };

export const RECENT_SEARCHES_KEY = "govtool_recent_searches";
const RECENT_LIMIT = 5;

/** The newest recent searches; empty when storage is unavailable or unreadable. */
export const readRecentSearches = (): RecentSearch[] => {
  try {
    const parsed = JSON.parse(localStorage.getItem(RECENT_SEARCHES_KEY) ?? "[]");
    return Array.isArray(parsed)
      ? parsed.filter(
        (item): item is RecentSearch =>
          typeof item?.query === "string" && typeof item?.kind === "string",
      )
      : [];
  } catch {
    return [];
  }
};

export const saveRecentSearch = (search: RecentSearch) => {
  const next = [
    search,
    ...readRecentSearches().filter((item) => item.query !== search.query),
  ].slice(0, RECENT_LIMIT);
  try {
    localStorage.setItem(RECENT_SEARCHES_KEY, JSON.stringify(next));
  } catch {
    // Storage full or blocked: recent searches are a convenience.
  }
  return next;
};

export const clearRecentSearches = () => {
  try {
    localStorage.removeItem(RECENT_SEARCHES_KEY);
  } catch {
    // Nothing to clear.
  }
};

/** "gov_action1q…7w4d": the first 12 and last 4 characters of a long id. */
export const shortId = (id: string) =>
  (id.length > 20 ? `${id.slice(0, 12)}…${id.slice(-4)}` : id);

/**
 * The forum's proposal search: names containing the phrase, newest first. Only
 * fields the forum backend's proposal allowlist accepts; it leaves out drafts
 * and old revisions on its own.
 */
export const forumSearchQuery = (phrase: string, page: number, pageSize: number) =>
  `filters[$and][0][prop_name][$containsi]=${encodeURIComponent(phrase)}` +
  `&pagination[page]=${page}&pagination[pageSize]=${pageSize}&sort[createdAt]=desc` +
  "&populate[0]=proposal_withdrawals";
