import { API } from "../API";

import type { DRepInfo } from "@models";

export const getDRepInfo = async (dRepID: string) => {
  const response = await API.get<DRepInfo>(`/drep/info/${dRepID}`);

  return response.data;
};
