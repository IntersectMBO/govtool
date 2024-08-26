import { DRepData, DRepMetadata, DrepDataDTO, MetadataStandard } from "@/models";
import { postValidate } from "@/services";

export const mapDtoToDrep = async (dto: DrepDataDTO): Promise<DRepData> => {
  const emptyMetadata = {
    paymentAddress: null,
    givenName: "",
    image: null,
    objectives: null,
    motivations: null,
    qualifications: null,
    references: [],
    doNotList: false,
    metadataStatus: null,
    metadataValid: true,
  };

  if (dto.metadataHash && dto.url) {
    const validationResponse = await postValidate<DRepMetadata>({
        url: dto.url,
        hash: dto.metadataHash,
        standard: MetadataStandard.CIP119,
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
