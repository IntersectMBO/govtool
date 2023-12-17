import { API } from "../API";
import { VotedProposal } from "@models";

export const getDRepVotes = async ({
  dRepId,
  filters,
  sorting,
}: {
  dRepId: string;
  filters: string[];
  sorting: string;
}) => {
  let urlBase = `/drep/getVotes/${dRepId}`;
  let urlParameters = "";
  if (filters.length > 0) {
    filters.forEach((item) => {
      urlParameters += `&type=${item}`;
    });
  }
  if (sorting.length) {
    urlParameters += `&sort=${sorting}`;
  }
  if (urlParameters.length) {
    urlParameters = urlParameters.replace("&", "?");
  }
  const response = await API.get<VotedProposal[]>(`${urlBase}${urlParameters}`);

  return response.data;
};
