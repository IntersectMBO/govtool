import { VotedProposal } from "@models";
import { API } from "../API";

export const getDRepVotes = async ({
  dRepID,
  filters,
  sorting,
}: {
  dRepID: string;
  filters: string[];
  sorting: string;
}) => {
  const urlBase = `/drep/getVotes/${dRepID}`;
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
