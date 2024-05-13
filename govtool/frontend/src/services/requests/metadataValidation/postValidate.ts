import type { MetadataValidationDTO, ValidateMetadataResult } from "@models";
import { API } from "@services";

export const postValidate = async (body: MetadataValidationDTO) => {
  const response = await API.post<ValidateMetadataResult>(
    `/metadata/validate`,
    body,
  );

  return response.data;
};
