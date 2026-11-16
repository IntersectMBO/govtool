import { VotedProposal } from "@models";
import { API } from "../API";
import { GovernanceActionsAPI } from "../GovernanceActionsAPI";

type GetDRepVotesParams = {
  type?: string[];
  sort?: string;
  search?: string;
};

export const getDRepVotes = async ({
  dRepID,
  params,
}: {
  dRepID: string;
  params: GetDRepVotesParams;
}): Promise<VotedProposal[]> => {
  const urlBase = `/drep/getVotes/${dRepID}`;

  const { data } = await API.get<VotedProposal[]>(urlBase, { params });

  return data;
};

/**
 * Every vote a given DRep cast, for its public profile. It goes through
 * GovernanceActionsAPI so a failure leaves the profile up instead of
 * navigating to the error page.
 */
export const getDRepVotesById = async (dRepId: string) => {
  const { data } = await GovernanceActionsAPI.get<VotedProposal[]>(
    `/drep/getVotes/${dRepId}`,
  );
  return data;
};
