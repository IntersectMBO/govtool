import type { MetadataValidationDTO, ValidateMetadataResult } from "@models";
import { METADATA_VALIDATION_API } from "../../API";

export const postValidate = async (body: MetadataValidationDTO) => {
  const response = await METADATA_VALIDATION_API.post<ValidateMetadataResult>(
    `/validate`,
    body,
  );

  return response.data;
};
