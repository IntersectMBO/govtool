import type { MetadataValidationDTO, ValidateMetadataResult } from "@models";
import axios from "axios";
import { env } from "@/config/env";

const TIMEOUT_IN_SECONDS = 30 * 1000; // 1000 ms is 1 s then its 30 s

// Not the shared API instance: its interceptor turns a 500 into the error
// page, and one card failing to validate should not leave the page.
const METADATA_API = axios.create({
  baseURL: env?.VITE_BASE_URL,
  timeout: TIMEOUT_IN_SECONDS,
});

export const postValidate = async <MetadataType>(
  body: MetadataValidationDTO,
) => {
  const response = await METADATA_API.post<
    ValidateMetadataResult<MetadataType>
  >(`/metadata/validate`, body);

  return response.data;
};
