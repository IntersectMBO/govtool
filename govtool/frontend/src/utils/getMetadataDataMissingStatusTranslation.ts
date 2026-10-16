import i18n from "@/i18n";
import { MetadataValidationStatus } from "@/models";

type DataMissingErrorKey =
  | "dataMissing"
  | "incorrectFormat"
  | "notVerifiable"
  | "urlBlocked"
  | "exceedsLimit"
  | "internalError";

/** The translation key suffix for each status, shared by every error text. */
export const METADATA_STATUS_ERROR_KEY: Record<
  MetadataValidationStatus,
  DataMissingErrorKey
> = {
  [MetadataValidationStatus.URL_NOT_FOUND]: "dataMissing",
  [MetadataValidationStatus.INVALID_JSONLD]: "incorrectFormat",
  [MetadataValidationStatus.INCORRECT_FORMAT]: "incorrectFormat",
  [MetadataValidationStatus.EXCEEDS_LIMIT]: "exceedsLimit",
  [MetadataValidationStatus.INVALID_HASH]: "notVerifiable",
  [MetadataValidationStatus.URL_BLOCKED]: "urlBlocked",
  [MetadataValidationStatus.INTERNAL_ERROR]: "internalError",
};

export const getMetadataStatusErrorKey = (
  status: MetadataValidationStatus,
): DataMissingErrorKey => METADATA_STATUS_ERROR_KEY[status] ?? "dataMissing";

/**
 * Retrieves the translation for the given metadata validation status.
 *
 * @param status - The metadata validation status.
 * @returns The translated string corresponding to the status.
 */
export const getMetadataDataMissingStatusTranslation = (
  status: MetadataValidationStatus,
): string => i18n.t(`dataMissingErrors.${getMetadataStatusErrorKey(status)}`);
