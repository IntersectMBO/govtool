import { API } from "../API";

import type { userInfo } from "@models";

export const getDRepInfo = async (dRepID: string) => {
  const response = await API.get<userInfo>(`/drep/info/${dRepID}`);

  return response.data;
};
