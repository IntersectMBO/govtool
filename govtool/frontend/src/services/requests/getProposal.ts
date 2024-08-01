import { ProposalData } from "@/models";
import { API } from "../API";
import { postValidate } from "./metadataValidation";

export const getProposal = async (proposalId: string, drepId?: string) => {
  const encodedHash = encodeURIComponent(proposalId);

  const { data } = await API.get<ProposalData>(
    `/proposal/get/${encodedHash}?drepId=${drepId}`,
  );

  if (data.metadataStatus || data.metadataValid) {
    return data;
  }
  if (!data.url || !data.metadataHash) {
    return data;
  }

  const validationResponse = await postValidate({
    url: data.url,
    hash: data.metadataHash,
  });

  return {
    ...data,
    title: validationResponse.metadata?.title,
    abstract: validationResponse.metadata?.abstract,
    motivation: validationResponse.metadata?.motivation,
    rationale: validationResponse.metadata?.rationale,
    references: validationResponse.metadata?.references,
    metadataStatus: validationResponse.status || null,
    metadataValid: validationResponse.valid,
  };
};
