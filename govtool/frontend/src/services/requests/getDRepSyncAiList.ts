import {
  type Infinite,
  DRepData,
} from "@models";
import { API } from "../API";


export const getDRepSyncAiList = async ({
  page = 0,
  pageSize = 10,
  searchPhrase = "",
}: any): Promise<any> => {
  const response = await API.get<any>("/drep-ai-search", {
    params: {
      page,
      pageSize,
      ...(searchPhrase && { search: searchPhrase }),
    },
  });

  console.log(response)

  const validatedResponse = {
    ...response.data,
    // elements: await Promise.all(
    //   response.data.elements.map(async (drep) => mapDtoToDrep(drep)),
    // ),
  };

  return validatedResponse;
};
