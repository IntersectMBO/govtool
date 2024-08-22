import { VotedProposal, VotedProposalDTO } from "@/models";
import { API } from "../API";
import { mapDtoToProposal } from "@/utils";

export const getProposal = async (
  proposalId: string,
  drepId?: string,
): Promise<VotedProposal> => {
  const encodedHash = encodeURIComponent(proposalId);

  const { data } = await API.get<VotedProposalDTO>(
    `/proposal/get/${encodedHash}?drepId=${drepId}`,
  );

    return {
      ...data,
      proposal: await mapDtoToProposal(data.proposal),
    };
};
