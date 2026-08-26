import axiosInstance from "@/lib/axiosInstance";
import type { ProposalData } from "@/models/api";
import { API } from "../API";
export const getProposals = async (): Promise<ProposalData[]> => {
  const { data } = await axiosInstance.get<ProposalData[]>(`/api/proposals`);
  return data;
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
  const response = await API.get<Infinite<ProposalData>>("/proposal/list", {
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

  return response.data;
};
