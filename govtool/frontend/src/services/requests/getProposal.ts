import { MetadataStandard, VotedProposal } from "@/models";
import { API } from "../API";
import { postValidate } from "./metadataValidation";

export const getProposal = async (
  proposalId: string,
  drepId?: string,
): Promise<VotedProposal> => {
  const encodedHash = encodeURIComponent(proposalId);

  const { data } = await API.get<VotedProposal>(
    `/proposal/get/${encodedHash}?drepId=${drepId}`,
  );

  if (data.proposal.metadataStatus || data.proposal.metadataValid) {
    return data;
  }
  if (!data.proposal.url || !data.proposal.metadataHash) {
    return data;
  }

  const validationResponse = await postValidate({
    url: data.proposal.url,
    hash: data.proposal.metadataHash,
    standard: MetadataStandard.CIP108,
  });

  return {
    ...data,
    proposal: {
      ...data.proposal,
      title: validationResponse.metadata?.title,
      abstract: validationResponse.metadata?.abstract,
      motivation: validationResponse.metadata?.motivation,
      rationale: validationResponse.metadata?.rationale,
      references: validationResponse.metadata?.references,
      metadataStatus: validationResponse.status || null,
      metadataValid: validationResponse.valid,
    },
  };
};
