import { VotedProposal, VotedProposalToDisplay } from "@models";
import { checkIsMissingGAMetadata } from "@utils";
import { API } from "../API";

type GetDRepVotesParams = {
  type?: string[];
  sort?: string;
  search?: string;
};

export const getDRepVotes = async ({
  dRepID,
  params,
}: {
  dRepID: string;
  params: GetDRepVotesParams;
}) => {
  const urlBase = `/drep/getVotes/${dRepID}`;

  const { data } = await API.get<VotedProposal[]>(urlBase, { params });

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
