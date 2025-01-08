import {
  type Infinite,
  type DRepStatus,
  type DRepListSort,
  DRepData,
  DrepDataDTO,
} from "@models";
import { API } from "../API";
import { dRepSearchPhraseProcessor, mapDtoToDrep } from "@/utils";

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
  searchPhrase: rawSearchPhrase = "",
  status = [],
}: GetDRepListArguments): Promise<Infinite<DRepData>> => {
  const searchPhrase = await dRepSearchPhraseProcessor(rawSearchPhrase);

  const response = await API.get<Infinite<DrepDataDTO>>("/drep/list", {
    params: {
      page,
      pageSize,
      ...(searchPhrase && { search: searchPhrase }),
      ...(filters.length && { type: filters }),
      ...(sorting && { sort: sorting }),
      ...(status.length && { status }),
    },
  });

  const validatedResponse = {
    ...response.data,
    elements: await Promise.all(
      response.data.elements.map(async (drep) => mapDtoToDrep(drep)),
    ),
  };

  return validatedResponse;
};
