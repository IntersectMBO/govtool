import { Infinite, ProposalData, ProposalDataDTO } from "@models";

import { API } from "../API";
import { mapDtoToProposal } from "@/utils";

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
}: GetProposalsArguments): Promise<Infinite<ProposalData>> => {
  const response = await API.get<Infinite<ProposalDataDTO>>("/proposal/list", {
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
      response.data.elements.map((proposalDTO) =>
        mapDtoToProposal(proposalDTO),
      ),
    ),
  };

  return validatedResponse;
};
