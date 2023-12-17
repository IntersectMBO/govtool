import { API } from "../API";

import type { DRepData } from "@models";

export const getDRepList = async () => {
  const response = await API.get<DRepData[]>("/drep/list");
  return response.data;
};
