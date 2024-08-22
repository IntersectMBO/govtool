import { EpochParams } from "@/models";

import { API } from "../API";

export const getEpochParams = async () => {
  const response = await API.get<EpochParams>("/epoch/params");

  return response.data;
};
