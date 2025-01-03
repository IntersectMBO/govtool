import { Infinite, ProposalData, ProposalDataDTO } from "@models";

import { API } from "../API";
import {
  decodeCIP129Identifier,
  getFullGovActionId,
  mapDtoToProposal,
} from "@/utils";

export type GetProposalsArguments = {
  dRepID?: string;
  filters?: string[];
  page?: number;
  pageSize?: number;
  sorting?: string;
  searchPhrase?: string;
  enabled?: boolean;
};

export const getProposals = async ({
  dRepID = "",
  filters = [],
  page = 0,
  // It allows fetch proposals and if we have 7 items, display 6 cards and "view all" button
  pageSize = 7,
  searchPhrase: rawSearchPhrase = "",
  sorting = "",
}: GetProposalsArguments): Promise<Infinite<ProposalData>> => {
  const searchPhrase = (() => {
    if (rawSearchPhrase.startsWith("gov_action")) {
      const { txID } = decodeCIP129Identifier(rawSearchPhrase);
      return getFullGovActionId(txID, 0);
    }

    return rawSearchPhrase;
  })();
  const response = await API.get<Infinite<ProposalDataDTO>>("/proposal/list", {
    params: {
      page,
      pageSize,
      ...(searchPhrase && {
        search: searchPhrase,
      }),
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
