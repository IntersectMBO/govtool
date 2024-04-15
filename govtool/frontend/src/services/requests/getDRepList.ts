import type { DRepData } from "@models";
import { API } from "../API";

export type GetDRepListParams = {
  search?: string;
  sort?: string;
  status?: string[];
};

export const getDRepList = async (params: GetDRepListParams) => {
  const response = await API.get<DRepData[]>("/drep/list", { params });
  return response.data;
};
