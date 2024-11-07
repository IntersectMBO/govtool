import {
  MetadataStandard,
  ProposalData,
  ProposalDataDTO,
  ProposalMetadata,
} from "@/models";
import { postValidate } from "@/services";

export const mapDtoToProposal = async (
  dto: ProposalDataDTO,
): Promise<ProposalData> => {
  if (dto.url && dto.metadataHash) {
    const validationResponse = await postValidate<ProposalMetadata>({
      url: dto.url,
      hash: dto.metadataHash,
      standard: MetadataStandard.CIP108,
    });

    return {
      ...dto,
      title: dto.title || validationResponse.metadata?.title,
      abstract: dto.abstract || validationResponse.metadata?.abstract,
      motivation: dto.motivation || validationResponse.metadata?.motivation,
      rationale: dto.rationale || validationResponse.metadata?.rationale,
      references: validationResponse.metadata?.references || [],
      metadataStatus: validationResponse.status || null,
      metadataValid: validationResponse.valid,
    };
  }

  return {
    ...dto,
    metadataStatus: null,
    metadataValid: true,
  };
};
