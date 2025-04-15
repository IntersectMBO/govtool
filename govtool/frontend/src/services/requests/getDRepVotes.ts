import { VotedProposal } from "@models";
import { API } from "../API";

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
