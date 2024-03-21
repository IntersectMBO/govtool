import type { DRepData } from "@models";
import { API } from "../API";

export const getDRepList = async (drepView?: string) => {
  const response = await API.get<DRepData[]>("/drep/list", drepView ? { params: { drepView } } : undefined);
  return response.data;
};
