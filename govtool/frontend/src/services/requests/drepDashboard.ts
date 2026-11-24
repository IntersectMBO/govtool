import axios from "axios";

import { env } from "@/config/env";
import type {
  DelegationHistoryRow,
  DRepDelegatorRow,
  DRepDiscussion,
  DRepHistoryRow,
  GovernanceActionRecord,
  Infinite,
  VotingPowerHistoryPoint,
} from "@/models";

import { GovernanceActionsAPI } from "../GovernanceActionsAPI";

// Reads for a DRep's own dashboard. All go through GovernanceActionsAPI (or the
// forum), so a failure stays in its panel instead of leaving the page.

const ACTIONS_PAGE = 100;
const ACTIONS_MAX_PAGES = 30;

/** Every governance action record, newest first, read page by page. */
export const getAllGovernanceActions = async () => {
  const read = async (
    page: number,
    acc: GovernanceActionRecord[],
  ): Promise<GovernanceActionRecord[]> => {
    const { data } = await GovernanceActionsAPI.get<GovernanceActionRecord[]>(
      "/governance-actions",
      {
        params: { search: "", filters: "", sort: "newestFirst", page, limit: ACTIONS_PAGE },
      },
    );
    const rows = Array.isArray(data) ? data : [];
    const all = [...acc, ...rows];
    return rows.length < ACTIONS_PAGE || page >= ACTIONS_MAX_PAGES
      ? all
      : read(page + 1, all);
  };
  return read(1, []);
};

const path = (value: string) => encodeURIComponent(value);

/** The DRep's delegators (drep.delegatorList). */
export const getDRepDelegators = async (
  drepId: string,
  page: number,
  pageSize: number,
) => {
  const { data } = await GovernanceActionsAPI.get<Infinite<DRepDelegatorRow>>(
    `/drep/${path(drepId)}/delegators`,
    { params: { page, pageSize, sort: "votingPower" } },
  );
  return data;
};

/** The DRep's registration, update, retirement and status events. */
export const getDRepHistory = async (
  drepId: string,
  page: number,
  pageSize: number,
  sort: "desc" | "asc",
) => {
  const { data } = await GovernanceActionsAPI.get<Infinite<DRepHistoryRow>>(
    `/drep/${path(drepId)}/history`,
    { params: { page, pageSize, sort } },
  );
  return data;
};

/** The governance delegations a stake address made, newest first. */
export const getDelegationHistory = async (stakeAddress: string) => {
  const { data } = await GovernanceActionsAPI.get<Infinite<DelegationHistoryRow>>(
    `/account/${path(stakeAddress)}/delegation-history`,
    { params: { page: 1, pageSize: 100 } },
  );
  return data;
};

/** The DRep's voting power and delegator count per epoch, oldest first. */
export const getDRepVotingPowerHistory = async (drepId: string) => {
  const { data } = await GovernanceActionsAPI.get<VotingPowerHistoryPoint[]>(
    `/drep/${path(drepId)}/voting-power-history`,
  );
  return Array.isArray(data) ? data : [];
};

type ForumComment = {
  id: number;
  attributes: {
    proposal_id: number | string;
    user_id: number | string;
    createdAt?: string;
    user_govtool_username?: string;
  };
};

type ForumThread = {
  id: number;
  attributes: {
    user_id?: number | string;
    user_govtool_username?: string;
    prop_likes?: number;
    prop_comments_number?: number;
    createdAt?: string;
    content?: {
      attributes?: {
        prop_name?: string;
        prop_abstract?: string | null;
        gov_action_type?: { attributes?: { gov_action_type_name?: string } } | null;
      };
    } | null;
  };
};

const forumGet = async <T>(query: string) => {
  const baseUrl = String(env.VITE_PDF_API_URL ?? "").replace(/\/+$/, "");
  const { data } = await axios.get<{ data?: T[] }>(`${baseUrl}/api/${query}`, {
    timeout: 30_000,
  });
  return Array.isArray(data?.data) ? data.data : [];
};

const time = (value?: string | null) => (value ? new Date(value).getTime() || 0 : 0);

/**
 * Forum threads a DRep posted or commented on. The forum ties a comment made
 * as a DRep to the DRep's key hash, which is how the DRep's forum account is
 * found without signing in; a DRep that never commented has no threads here.
 */
export const getDRepDiscussions = async (dRepHash: string): Promise<DRepDiscussion[]> => {
  if (!/^[0-9a-f]{56}$/i.test(dRepHash)) return [];
  const comments = await forumGet<ForumComment>(
    `comments?filters[drep_id][$eq]=${dRepHash.toLowerCase()}` +
      "&sort[createdAt]=desc&pagination[page]=1&pagination[pageSize]=100",
  );
  if (!comments.length) return [];

  const userId = String(comments[0].attributes.user_id);
  const ownName = comments[0].attributes.user_govtool_username ?? "";
  const lastOwnReply = new Map<number, string>();
  comments.forEach(({ attributes }) => {
    const id = Number(attributes.proposal_id);
    if (Number.isInteger(id) && attributes.createdAt && !lastOwnReply.has(id)) {
      lastOwnReply.set(id, attributes.createdAt);
    }
  });
  const repliedIds = [...lastOwnReply.keys()].slice(0, 100);

  const [authored, replied] = await Promise.all([
    /^\d+$/.test(userId)
      ? forumGet<ForumThread>(
        `proposals?filters[user_id][$eq]=${userId}` +
            "&sort[createdAt]=desc&pagination[page]=1&pagination[pageSize]=50",
      )
      : Promise.resolve([]),
    repliedIds.length
      ? forumGet<ForumThread>(
        `proposals?${repliedIds.map((id, i) => `filters[prop_id][$in][${i}]=${id}`).join("&")}` +
            // A prop_id filter turns off the forum's own active-revision rule.
            "&filters[prop_rev_active][$eq]=true&sort[createdAt]=desc" +
            `&pagination[page]=1&pagination[pageSize]=${repliedIds.length}`,
      )
      : Promise.resolve([]),
  ]);

  const seen = new Set<number>();
  return [...authored, ...replied]
    .filter(({ id }) => (seen.has(id) ? false : (seen.add(id), true)))
    .map(({ id, attributes }): DRepDiscussion => {
      const content = attributes.content?.attributes;
      const own = lastOwnReply.get(id);
      return {
        id,
        title: content?.prop_name ?? "",
        abstract: content?.prop_abstract ?? "",
        topic: content?.gov_action_type?.attributes?.gov_action_type_name ?? null,
        author: attributes.user_govtool_username ?? "",
        isOwn: String(attributes.user_id) === userId,
        createdAt: attributes.createdAt ?? null,
        replies: attributes.prop_comments_number ?? 0,
        likes: attributes.prop_likes ?? 0,
        repliedByYou: !!own,
        lastReply: own ? { author: ownName, at: own, isOwn: true } : null,
      };
    })
    .sort(
      (a, b) =>
        Math.max(time(b.createdAt), time(b.lastReply?.at)) -
        Math.max(time(a.createdAt), time(a.lastReply?.at)),
    );
};
