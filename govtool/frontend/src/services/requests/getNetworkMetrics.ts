import { NetworkMetrics } from "@models";
import { API } from "../API";

export const getNetworkMetrics = async () => {
  const response = await API.get<NetworkMetrics>("/network/metrics");

  return response.data;
};
