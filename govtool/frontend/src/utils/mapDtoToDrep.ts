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
    imageUrl: null,
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

  // We need to prefetch the image, for the  IPFS support
  let base64Image = null;
  const isIPFSImage = dto.imageUrl?.startsWith("ipfs://") || false;
  if (dto.imageUrl) {
    fetch(
      isIPFSImage
        ? `${process.env.VITE_IPFS_GATEWAY}/${dto.imageUrl?.slice(7)}`
        : dto.imageUrl,
      isIPFSImage
        ? {
            headers: { project_id: import.meta.env.VITE_IPFS_PROJECT_ID },
          }
        : {},
    )
      .then(async (res) => {
        const blob = await res.blob();
        const reader = new FileReader();
        reader.readAsDataURL(blob);
        reader.onloadend = () => {
          base64Image = reader.result;
        };
      })
      .catch((e) => {
        console.error("Fetching the DRep image failed, reason: ", e);
      });
  }

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
      image: isIPFSImage ? base64Image : dto.imageUrl,
      view,
    };
  }

  return {
    ...dto,
    ...emptyMetadata,
    view,
    image: isIPFSImage ? base64Image : dto.imageUrl,
  };
};
