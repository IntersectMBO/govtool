import {
  type Infinite,
  type DRepStatus,
  type DRepListSort,
  DRepData,
} from "@models";
import { API } from "../API";
// import { mapDtoToDrep } from "@/utils";

// export type GetDRepListArguments = {
//   filters?: string[];
//   page?: number;
//   pageSize?: number;
//   sorting?: DRepListSort;
//   status?: DRepStatus[];
//   searchPhrase?: string;
// };

export const getDRepSyncAiList = async ({
  page = 0,
  pageSize = 10,
  searchPhrase = "",
}: any): Promise<Infinite<DRepData>> => {
  const response = await API.get<Infinite<DRepData>>("/drep-ai-search", {
    params: {
      page,
      pageSize,
      ...(searchPhrase && { search: searchPhrase }),
    },
  });

  const validatedResponse = {
    ...response.data,
    // elements: await Promise.all(
    //   response.data.elements.map(async (drep) => mapDtoToDrep(drep)),
    // ),
  };

  return validatedResponse;
};
