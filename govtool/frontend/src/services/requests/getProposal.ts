import { VotedProposal } from "@/models";
import { decodeCIP129Identifier } from "@/utils";

import { API } from "../API";

export const getProposal = async (
  proposalId: string,
  drepId?: string,
): Promise<VotedProposal> => {
  const isCIP129Identifier = proposalId.includes("gov_action");
  if (isCIP129Identifier) {
    const { txID } = decodeCIP129Identifier(proposalId);
    proposalId = txID;
  }

  const encodedHash = encodeURIComponent(proposalId);

  const { data } = await API.get<VotedProposal>(
    `/proposal/get/${encodedHash}?drepId=${drepId}`,
  );

  return data;
};
