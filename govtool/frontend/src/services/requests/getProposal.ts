import { VotedProposal } from "@/models";
import { decodeCIP129Identifier } from "@/utils";

import { API } from "../API";

export const getProposal = async (
  proposalId: string,
  drepId?: string,
): Promise<VotedProposal> => {
  const isCIP129Identifier = proposalId.startsWith("gov_action");
  if (isCIP129Identifier) {
    const { txID, index } = decodeCIP129Identifier(proposalId);
    proposalId = `${txID}#${parseInt(index, 16)}`;
  }

  const encodedHash = encodeURIComponent(proposalId);

  const { data } = await API.get<VotedProposal>(
    `/proposal/get/${encodedHash}?drepId=${drepId}`,
  );

  return data;
};
