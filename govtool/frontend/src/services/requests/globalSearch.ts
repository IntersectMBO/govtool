import axios from "axios";

import { env } from "@/config/env";
import type {
  Account,
  CurrentDelegation,
  DRepData,
  ForumProposal,
  Infinite,
  ProposalData,
  StakeAddressLookup,
} from "@/models";
import {
  dRepSearchPhraseProcessor,
  fixViewForScriptBasedDRep,
  forumSearchQuery,
} from "@/utils";

import { GovernanceActionsAPI } from "../GovernanceActionsAPI";

// Reads for the header's global search. They go through GovernanceActionsAPI,
// so a failed search shows in the dialog instead of opening the error page.

/**
 * Live governance actions matching a title, keyword or action id. The backend
 * matches a CIP-129 id itself, index included. 0-based page.
 */
export const searchGovernanceActions = async (
  phrase: string,
  page: number,
  pageSize: number,
) => {
  const { data } = await GovernanceActionsAPI.get<Infinite<ProposalData>>("/proposal/list", {
    params: { page, pageSize, search: phrase },
  });
  return data;
};

// Phrases worth turning into a DRep id first: a $handle, a drep1… id or hex.
const DREP_ID_SHAPED = /^(\$|drep)|^[0-9a-f]{56,58}$/i;

/**
 * Listed DReps matching a name, DRep id or $handle. 0-based page. Only id- or
 * handle-shaped phrases go to handle.me, and rows are not run through
 * mapDtoToDrep, which would fetch every DRep's image from its own host.
 */
export const searchDReps = async (phrase: string, page: number, pageSize: number) => {
  const search = DREP_ID_SHAPED.test(phrase) ? await dRepSearchPhraseProcessor(phrase) : phrase;
  const { data } = await GovernanceActionsAPI.get<Infinite<DRepData>>("/drep/list", {
    params: { page, pageSize, search, listed: true },
  });
  return {
    ...data,
    elements: data.elements.map((dRep) => ({
      ...dRep,
      view: fixViewForScriptBasedDRep(dRep.view, dRep.isScriptBased),
    })),
  };
};

/** Forum proposals whose name contains the phrase. 1-based page. */
export const searchForumProposals = async (
  phrase: string,
  page: number,
  pageSize: number,
) => {
  const baseUrl = String(env.VITE_PDF_API_URL ?? "").replace(/\/+$/, "");
  const { data } = await axios.get<{
    data: ForumProposal[];
    meta?: { pagination?: { total?: number } };
  }>(`${baseUrl}/api/proposals?${forumSearchQuery(phrase, page, pageSize)}`, {
    timeout: 30_000,
  });
  return {
    elements: data?.data ?? [],
    total: data?.meta?.pagination?.total ?? data?.data?.length ?? 0,
  };
};

const settled = <T>(result: PromiseSettledResult<T>) =>
  (result.status === "fulfilled" ? result.value : undefined);

/**
 * Registration, delegation and voting power of a stake address. A part the
 * backend cannot answer is left undefined; all three failing is an error.
 */
export const lookupStakeAddress = async (
  stakeAddress: string,
): Promise<StakeAddressLookup> => {
  const path = encodeURIComponent(stakeAddress);
  const [account, delegation, votingPower] = await Promise.allSettled([
    GovernanceActionsAPI.get<Account>(`/account/${path}`),
    GovernanceActionsAPI.get<CurrentDelegation | null>(
      `/ada-holder/get-current-delegation/${path}`,
    ),
    GovernanceActionsAPI.get<number>(`/ada-holder/get-voting-power/${path}`),
  ]);
  if ([account, delegation, votingPower].every((r) => r.status === "rejected")) {
    throw (account as PromiseRejectedResult).reason;
  }
  const current = settled(delegation)?.data;
  // /account answers 404 for a stake address the chain has never registered.
  const neverSeen =
    account.status === "rejected" &&
    (account.reason as { response?: { status?: number } })?.response?.status === 404;
  return {
    stakeAddress,
    isRegistered: neverSeen ? false : settled(account)?.data?.isRegistered,
    delegation: current
      ? {
        ...current,
        dRepView:
            current.dRepView &&
            fixViewForScriptBasedDRep(current.dRepView, current.isDRepScriptBased),
      }
      : current,
    votingPower: settled(votingPower)?.data,
  };
};
