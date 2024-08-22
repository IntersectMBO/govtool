import { VotedProposal, VotedProposalDTO } from "@models";
import { API } from "../API";
import { mapDtoToProposal } from "@/utils";

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

  const { data } = await API.get<VotedProposalDTO[]>(urlBase, { params });

  const validatedData = await Promise.all(
    data.map(async (votedProposal) => ({
      ...votedProposal,
      proposal: await mapDtoToProposal(votedProposal.proposal),
    }))
  );

  return validatedData;
};
