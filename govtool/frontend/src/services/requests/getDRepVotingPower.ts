import { API } from "../API";

export const getDRepVotingPower = async ({ dRepID }: { dRepID: string }) => {
  const response = await API.get<number>(`/drep/get-voting-power/${dRepID}`);
  return response.data;
};
