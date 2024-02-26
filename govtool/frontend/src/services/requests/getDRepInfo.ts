import { API } from "../API";

import type { VoterInfo } from "@models";

export const getDRepInfo = async (dRepID: string) => {
  const response = await API.get<VoterInfo>(`/drep/info/${dRepID}`);

  return response.data;
};
