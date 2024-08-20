import type { MetadataValidationDTO, ValidateMetadataResult } from "@models";
import axios from "axios";

const TIMEOUT_IN_SECONDS = 30 * 1000; // 1000 ms is 1 s then its 30 s

const METADATA_API = axios.create({
  // API address of the metadata-validation service is different from the main API
  baseURL: import.meta.env?.VITE_METADATA_API_URL,
  timeout: TIMEOUT_IN_SECONDS,
});

export const postValidate = async <MetadataType>(body: MetadataValidationDTO) => {
  const response = await METADATA_API.post<ValidateMetadataResult<MetadataType>>(
    `/validate`,
    body,
  );

  return response.data;
};
