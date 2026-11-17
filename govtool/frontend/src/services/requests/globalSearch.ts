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
  decodeCIP129Identifier,
  dRepSearchPhraseProcessor,
  fixViewForScriptBasedDRep,
  forumSearchQuery,
  getFullGovActionId,
  mapDtoToDrep,
} from "@/utils";

import { GovernanceActionsAPI } from "../GovernanceActionsAPI";

// Reads for the header's global search. They go through GovernanceActionsAPI,
// so a failed search shows in the dialog instead of opening the error page.

/** Live governance actions matching a title, keyword or action id. 0-based page. */
export const searchGovernanceActions = async (
  phrase: string,
  page: number,
  pageSize: number,
) => {
  let search = phrase;
  if (phrase.startsWith("gov_action")) {
    try {
      search = getFullGovActionId(decodeCIP129Identifier(phrase).txID, 0);
    } catch {
      search = phrase;
    }
  }
  const { data } = await GovernanceActionsAPI.get<Infinite<ProposalData>>("/proposal/list", {
    params: { page, pageSize, search },
  });
  return data;
};

/** Listed DReps matching a name, DRep id or $handle. 0-based page. */
export const searchDReps = async (phrase: string, page: number, pageSize: number) => {
  const search = await dRepSearchPhraseProcessor(phrase);
  const { data } = await GovernanceActionsAPI.get<Infinite<DRepData>>("/drep/list", {
    params: { page, pageSize, search, listed: true },
  });
  return {
    ...data,
    elements: await Promise.all(data.elements.map((dRep) => mapDtoToDrep(dRep))),
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
  return {
    stakeAddress,
    isRegistered: settled(account)?.data?.isRegistered,
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
