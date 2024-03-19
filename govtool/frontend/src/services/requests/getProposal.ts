import { API } from "../API";

export const getProposal = async (proposalId: string, drepId?: string) => {
  const encodedHash = encodeURIComponent(proposalId);

  const response = await API.get(
    `/proposal/get/${encodedHash}?drepId=${drepId}`,
  );

  return response.data;
};
