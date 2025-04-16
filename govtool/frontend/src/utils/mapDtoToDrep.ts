import { DRepData } from "@/models";
import { fixViewForScriptBasedDRep } from "./dRep";

const imageFetchDefaultOptions: RequestInit = {
  mode: "no-cors",
};

export const mapDtoToDrep = async (dto: DRepData): Promise<DRepData> => {
  // DBSync contains wrong representation of DRep view for script based DReps
  const view = fixViewForScriptBasedDRep(dto.view, dto.isScriptBased);

  // We need to prefetch the image, for the  IPFS support
  let base64Image = null;
  const isIPFSImage = dto.imageUrl?.startsWith("ipfs://") || false;
  if (dto.imageUrl) {
    // eslint-disable-next-line no-console
    console.debug("Fetching image", dto.imageUrl);
    fetch(
      isIPFSImage
        ? `${import.meta.env.VITE_IPFS_GATEWAY}/${dto.imageUrl?.slice(7)}`
        : dto.imageUrl,
      isIPFSImage
        ? {
            ...imageFetchDefaultOptions,
            headers: { project_id: import.meta.env.VITE_IPFS_PROJECT_ID },
          }
        : // set request mode no-cors
          {
            ...imageFetchDefaultOptions,
          },
    )
      .then(async (res) => {
        const blob = await res.blob();
        const reader = new FileReader();
        reader.readAsDataURL(blob);
        reader.onloadend = () => {
          base64Image = reader.result;
        };
      })
      .catch((error) => {
        if (import.meta.env.VITE_IS_DEV) {
          console.error("Error fetching image", error);
        }
      });
  }
  return {
    ...dto,
    view,
    image: isIPFSImage ? base64Image : dto.imageUrl,
  };
};
