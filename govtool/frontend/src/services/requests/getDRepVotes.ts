import { MetadataStandard, VotedProposal } from "@models";
import { API } from "../API";
import { postValidate } from "./metadataValidation";

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

  const validatedData = await Promise.all(
    data.map(async (votedProposal) => {
      if (
        votedProposal.proposal.metadataStatus ||
        votedProposal.proposal.metadataValid
      ) {
        return votedProposal;
      }

      if (votedProposal.proposal.url && votedProposal.proposal.metadataHash) {
        const validationResponse = await postValidate({
          url: votedProposal.proposal.url,
          hash: votedProposal.proposal.metadataHash,
          standard: MetadataStandard.CIP108,
        });

        return {
          ...votedProposal,
          proposal: {
            ...votedProposal.proposal,
            abstract: validationResponse.metadata?.abstract,
            motivation: validationResponse.metadata?.motivation,
            title: validationResponse.metadata?.title,
            rationale: validationResponse.metadata?.rationale,
            references: validationResponse.metadata?.references,
            metadataStatus: validationResponse.status || null,
            metadataValid: validationResponse.valid,
          },
        };
      }
    }),
  );

  return validatedData;
};
