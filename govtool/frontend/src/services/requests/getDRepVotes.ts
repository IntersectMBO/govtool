import { VotedProposal, VotedProposalToDisplay } from "@models";
import { checkIsMissingGAMetadata } from "@utils";
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
  const { data } = await API.get<VotedProposal[]>(`${urlBase}${urlParameters}`);

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
