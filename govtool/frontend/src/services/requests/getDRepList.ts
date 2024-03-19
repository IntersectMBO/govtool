import type { DRepData } from "@models";
import { API } from "../API";

export const getDRepList = async () => {
  const response = await API.get<DRepData[]>("/drep/list");
  return response.data;
};
