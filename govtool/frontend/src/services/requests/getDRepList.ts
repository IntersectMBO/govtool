import { bech32 } from "bech32";
import {
  type Infinite,
  type DRepStatus,
  type DRepListSort,
  DRepData,
  DrepDataDTO,
} from "@models";
import { API } from "../API";
import {
  decodeCIP129Identifier,
  encodeCIP129Identifier,
  mapDtoToDrep,
} from "@/utils";

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
  const searchPhraseProcessor = async () => {
    try {
      if (rawSearchPhrase.startsWith("drep_script")) {
        const { words } = bech32.decode(rawSearchPhrase);
        return bech32.encode("drep", words);
      }
      if (rawSearchPhrase.startsWith("drep")) {
        const decodedIdentifier = decodeCIP129Identifier(rawSearchPhrase);
        if (decodedIdentifier) {
          const isCIP129Identifier = decodedIdentifier.txID.startsWith("22");
          if (isCIP129Identifier) {
            return encodeCIP129Identifier({
              txID: decodedIdentifier.txID.slice(2),
              bech32Prefix: "drep",
            });
          }
          return encodeCIP129Identifier({
            txID: decodedIdentifier.txID,
            bech32Prefix: "drep",
          });
        }
      }
      return rawSearchPhrase;
    } catch (e) {
      return rawSearchPhrase;
    }
  };

  const searchPhrase = await searchPhraseProcessor();

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
