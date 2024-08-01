import { InfinityProposals, MetadataStandard } from "@models";

import { postValidate } from "./metadataValidation";
import { API } from "../API";

export type GetProposalsArguments = {
  dRepID?: string;
  filters?: string[];
  page?: number;
  pageSize?: number;
  sorting?: string;
  searchPhrase?: string;
};

export const getProposals = async ({
  dRepID = "",
  filters = [],
  page = 0,
  // It allows fetch proposals and if we have 7 items, display 6 cards and "view all" button
  pageSize = 7,
  searchPhrase = "",
  sorting = "",
}: GetProposalsArguments): Promise<InfinityProposals> => {
  const response = await API.get<InfinityProposals>("/proposal/list", {
    params: {
      page,
      pageSize,
      ...(searchPhrase && { search: searchPhrase }),
      ...(filters.length && { type: filters }),
      ...(sorting && { sort: sorting }),
      ...(dRepID && { drepId: dRepID }),
    },
  });

  const validatedResponse = {
    ...response.data,
    elements: await Promise.all(
      response.data.elements.map(async (proposal) => {
        if (proposal.metadataStatus || proposal.metadataValid) {
          return {
            ...proposal,
            metadataStatus: proposal.metadataStatus,
            metadataValid: proposal.metadataValid,
          };
        }
        if (proposal.url && proposal.metadataHash) {
          const validationResponse = await postValidate({
            url: proposal.url,
            hash: proposal.metadataHash,
            standard: MetadataStandard.CIP108,
          });
          return {
            ...proposal,
            abstract: validationResponse.metadata?.abstract,
            motivation: validationResponse.metadata?.motivation,
            title: validationResponse.metadata?.title,
            rationale: validationResponse.metadata?.rationale,
            references: validationResponse.metadata?.references,
            metadataStatus: validationResponse.status || null,
            metadataValid: validationResponse.valid,
          };
        }

        return proposal;
      }),
    ),
  };
  return validatedResponse;
};
