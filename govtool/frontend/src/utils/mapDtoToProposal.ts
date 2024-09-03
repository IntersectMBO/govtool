import { ProposalData, ProposalDataDTO, ProposalMetadata } from "@/models";
import { postValidate } from "@/services";

export const mapDtoToProposal = async (
  dto: ProposalDataDTO,
): Promise<ProposalData> => {
  if (dto.url && dto.metadataHash) {
    const validationResponse = await postValidate<ProposalMetadata>({
      url: dto.url,
      hash: dto.metadataHash,
    });

    return {
      ...dto,
      title: validationResponse.metadata?.title,
      abstract: validationResponse.metadata?.abstract,
      motivation: validationResponse.metadata?.motivation,
      rationale: validationResponse.metadata?.rationale,
      references: validationResponse.metadata?.references,
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
