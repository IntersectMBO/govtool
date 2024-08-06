import { DRepData, DRepMetadata, DrepDataDTO, MetadataStandard } from "@/models";
import { postValidate } from "@/services";

export const mapDtoToDrep = async (dto: DrepDataDTO): Promise<DRepData> => {
  const emptyMetadata = {
    bio: null,
    dRepName: null,
    email: null,
    references: [],
    metadataStatus: null,
    metadataValid: true,
  };

  if (dto.metadataHash && dto.url) {
    const validationResponse = await postValidate<DRepMetadata>({
        url: dto.url,
        hash: dto.metadataHash,
        standard: MetadataStandard.CIPQQQ,
      });
    return {
      ...dto,
      ...emptyMetadata,
      ...validationResponse.metadata,
      metadataStatus: validationResponse.status || null,
      metadataValid: validationResponse.valid,
    };
  }

  return {
    ...dto,
    ...emptyMetadata,
  };
};
