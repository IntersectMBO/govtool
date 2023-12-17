import { API } from "../API";

export const getDRepVotingPower = async ({ dRepId }: { dRepId: string }) => {
  const response = await API.get<number>(`/drep/get-voting-power/${dRepId}`);
  return response.data;
};
