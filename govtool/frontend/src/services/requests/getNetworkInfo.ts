import { NetworkInfo } from "@/models";
import { API } from "../API";

export const getNetworkInfo = async () => {
  const response = await API.get<NetworkInfo>("/network/info");

  return response.data;
};
