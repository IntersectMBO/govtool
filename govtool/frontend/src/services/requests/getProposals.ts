import { API } from "../API";

export type getProposalsArguments = {
  dRepID?: string;
  filters?: string[];
  page?: number;
  pageSize?: number;
  sorting?: string;
};

export const getProposals = async ({
  dRepID = "",
  filters = [],
  page = 0,
  // It allows fetch proposals and if we have 7 items, display 6 cards and "view all" button
  pageSize = 7,
  sorting = "",
}: getProposalsArguments) => {
  let urlBase = "/proposal/list";
  let urlParameters = `?page=${page}&pageSize=${pageSize}`;

  if (filters.length > 0) {
    filters.forEach((item) => {
      urlParameters += `&type=${item}`;
    });
  }
  if (sorting.length) {
    urlParameters += `&sort=${sorting}`;
  }
  if (dRepID) {
    urlParameters += `&drepId=${dRepID}`;
  }

  const response = await API.get(`${urlBase}${urlParameters}`);
  return response.data;
};
