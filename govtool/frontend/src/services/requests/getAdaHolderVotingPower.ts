import { API } from "../API";

export const getAdaHolderVotingPower = async ({
  stakeKey,
}: {
  stakeKey?: string;
}) => {
  const response = await API.get(`/ada-holder/get-voting-power/${stakeKey}`);

  return response.data;
};
