import type { InfinityDRepData, DRepStatus, DRepListSort } from "@models";
import { API } from "../API";

export type GetDRepListArguments = {
  filters?: string[];
  page?: number;
  pageSize?: number;
  sorting?: DRepListSort;
  status?: DRepStatus[];
  searchPhrase?: string;
};

export const getDRepList = async ({
  sorting,
  filters = [],
  page = 0,
  pageSize = 10,
  searchPhrase = "",
  status = [],
}: GetDRepListArguments): Promise<InfinityDRepData> => {
  const response = await API.get("/drep/list", {
    params: {
      page,
      pageSize,
      ...(searchPhrase && { search: searchPhrase }),
      ...(filters.length && { type: filters }),
      ...(sorting && { sort: sorting }),
      ...(status.length && { status }),
    },
  });
  return response.data;
};
