import { API } from "../API";

export const getProposals = async (
  filters: string[],
  sorting: string,
  page: number = 0,
  pageSize: number = 7
) => {
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

  const response = await API.get(`${urlBase}${urlParameters}`);
  return response.data;
};
