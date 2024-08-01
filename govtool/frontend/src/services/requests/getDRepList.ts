import {
  type InfinityDRepData,
  type DRepStatus,
  type DRepListSort,
  MetadataStandard,
} from "@models";
import { API } from "../API";
import { postValidate } from "./metadataValidation";

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
  const response = await API.get<InfinityDRepData>("/drep/list", {
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
      response.data.elements.map(async (drep) => {
        if (drep.metadataStatus || drep.metadataValid) {
          return drep;
        }
        if (drep.url && drep.metadataHash) {
          const validationResponse = await postValidate({
            url: drep.url,
            hash: drep.metadataHash,
            standard: MetadataStandard.CIPQQQ,
          });
          return {
            ...drep,
            bio: validationResponse.metadata?.bio,
            dRepName: validationResponse.metadata?.dRepName,
            email: validationResponse.metadata?.email,
            references: validationResponse.metadata?.references,
            metadataStatus: validationResponse.status || null,
            metadataValid: validationResponse.valid,
          };
        }
        return drep;
      }),
    ),
  };

  return validatedResponse;
};
