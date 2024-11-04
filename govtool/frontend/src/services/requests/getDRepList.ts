import { bech32 } from "bech32";

import {
  type Infinite,
  type DRepStatus,
  type DRepListSort,
  DRepData,
  DrepDataDTO,
} from "@models";
import { API } from "../API";
import { mapDtoToDrep } from "@/utils";

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
  // DBSync contains wrong representation of DRep view for script based DReps,
  // but it's still used by BE
  const searchPhrase = (() => {
    if (rawSearchPhrase.startsWith("drep_script")) {
      const { words } = bech32.decode(rawSearchPhrase);
      return bech32.encode("drep", words);
    }
    return rawSearchPhrase;
  })();

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
