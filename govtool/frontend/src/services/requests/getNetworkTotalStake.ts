import { NetworkTotalStake } from "@models";
import { API } from "../API";

export const getNetworkTotalStake = async () => {
  const response = await API.get<NetworkTotalStake>("/network/total-stake");

  return response.data;
};
