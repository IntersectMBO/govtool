import {
  DRepData,
  DRepMetadata,
  DrepDataDTO,
  MetadataStandard,
} from "@/models";
import { postValidate } from "@/services";
import { fixViewForScriptBasedDRep } from "./dRep";

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

  // DBSync contains wrong representation of DRep view for script based DReps
  const view = fixViewForScriptBasedDRep(dto.view, dto.isScriptBased);

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
      view,
    };
  }

  return {
    ...dto,
    ...emptyMetadata,
    view,
  };
};
