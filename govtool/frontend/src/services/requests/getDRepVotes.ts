import { VotedProposal, VotedProposalToDisplay } from "@models";
import { API } from "../API";
import { checkIsMissingGAMetadata } from "@utils";

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

  const data = response.data;

  const mappedData = (await Promise.all(
    data.map(async (proposal) => {
      const isDataMissing = await checkIsMissingGAMetadata({
        hash: proposal?.proposal?.metadataHash,
        url: proposal?.proposal?.url,
      });

      return {
        vote: proposal.vote,
        proposal: { ...proposal.proposal, isDataMissing },
      };
    }),
  )) as VotedProposalToDisplay[];

  return mappedData;
};
