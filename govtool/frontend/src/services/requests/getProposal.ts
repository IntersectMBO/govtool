import { API } from "../API";

export const getProposal = async (proposalId: string, drepId?: string) => {
  const encodedHash = encodeURIComponent(proposalId);

  const { data } = await API.get(
    `/proposal/get/${encodedHash}?drepId=${drepId}`,
  );

  return data;
};
