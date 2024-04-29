import type { VoterInfo } from "@models";
import { API } from "../API";

export const getVoterInfo = async (dRepID: string) => {
  const response = await API.get<VoterInfo>(`/drep/info/${dRepID}`);

  return response.data;
};
