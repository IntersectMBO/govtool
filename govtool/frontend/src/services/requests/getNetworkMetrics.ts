import { API } from "../API";

export const getNetworkMetrics = async () => {
  const response = await API.get("/network/metrics");

  return response.data;
};
