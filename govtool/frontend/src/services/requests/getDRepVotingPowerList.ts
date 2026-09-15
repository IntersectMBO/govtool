import { DRepVotingPowerListResponse } from "@/models";
import { API } from "../API";

export const getDRepVotingPowerList = async (
  identifiers: string[],
): Promise<DRepVotingPowerListResponse> => {
  const params = new URLSearchParams();
  identifiers.forEach((id: string) => params.append("identifiers", id));

  const response = await API.get<DRepVotingPowerListResponse>(
    `/drep/voting-power-list?${params.toString()}`,
  );

  return response.data;
};
