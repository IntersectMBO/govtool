import i18n from "@/i18n";
import { MetadataValidationStatus } from "@/models";

/**
 * Retrieves the translation for the given metadata validation status.
 *
 * @param status - The metadata validation status.
 * @returns The translated string corresponding to the status.
 */
export const getMetadataDataMissingStatusTranslation = (
  status: MetadataValidationStatus,
): string => {
  const errorKey = {
    [MetadataValidationStatus.URL_NOT_FOUND]: "dataMissing",
    [MetadataValidationStatus.INVALID_JSONLD]: "incorrectFormat",
    [MetadataValidationStatus.INVALID_HASH]: "notVerifiable",
  }[status] as "dataMissing" | "incorrectFormat" | "notVerifiable";
  return i18n.t(`dataMissingErrors.${errorKey || "dataMissing"}`);
};
