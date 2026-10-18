import { ModalState } from "@/context";
import I18n from "@/i18n";
import { MetadataValidationStatus } from "@/models";

export enum MetadataHashValidationErrors {
  INVALID_URL = "Invalid URL",
  INVALID_JSON = "Invalid JSON",
  INVALID_HASH = "Invalid hash",
  FETCH_ERROR = "Error fetching data",
}

const externalDataDoesntMatchModal = {
  status: "warning",
  title: I18n.t("modals.externalDataDoesntMatch.title"),
  message: I18n.t("modals.externalDataDoesntMatch.message"),
  buttonText: I18n.t("modals.externalDataDoesntMatch.buttonText"),
  cancelText: I18n.t("modals.externalDataDoesntMatch.cancelRegistrationText"),
  feedbackText: I18n.t("modals.externalDataDoesntMatch.feedbackText"),
} as const;

const urlCannotBeFound = {
  status: "warning",
  title: I18n.t("modals.urlCannotBeFound.title"),
  message: I18n.t("modals.urlCannotBeFound.message"),
  link: "https://docs.gov.tools",
  linkText: I18n.t("modals.urlCannotBeFound.linkText"),
  buttonText: I18n.t("modals.urlCannotBeFound.buttonText"),
  cancelText: I18n.t("modals.urlCannotBeFound.cancelRegistrationText"),
  feedbackText: I18n.t("modals.urlCannotBeFound.feedbackText"),
};

/** A warning modal whose texts all live under `modals.<key>`. */
const storageErrorModal = (
  key:
    | "externalDataIncorrectFormat"
    | "externalDataTooLarge"
    | "urlBlocked"
    | "metadataCheckFailed",
) =>
  ({
    status: "warning",
    title: I18n.t(`modals.${key}.title`),
    message: I18n.t(`modals.${key}.message`),
    buttonText: I18n.t(`modals.${key}.buttonText`),
    cancelText: I18n.t(`modals.${key}.cancelRegistrationText`),
    feedbackText: I18n.t(`modals.${key}.feedbackText`),
  }) as const;

const externalDataIncorrectFormatModal = storageErrorModal(
  "externalDataIncorrectFormat",
);

export const storageInformationErrorModals: Record<
  MetadataValidationStatus,
  ModalState<
    typeof externalDataDoesntMatchModal | typeof urlCannotBeFound
  >["state"]
> = {
  [MetadataValidationStatus.URL_NOT_FOUND]: urlCannotBeFound,
  [MetadataValidationStatus.INCORRECT_FORMAT]: externalDataIncorrectFormatModal,
  [MetadataValidationStatus.INVALID_JSONLD]: externalDataIncorrectFormatModal,
  [MetadataValidationStatus.INVALID_HASH]: externalDataDoesntMatchModal,
  [MetadataValidationStatus.EXCEEDS_LIMIT]: storageErrorModal(
    "externalDataTooLarge",
  ),
  [MetadataValidationStatus.URL_BLOCKED]: storageErrorModal("urlBlocked"),
  [MetadataValidationStatus.INTERNAL_ERROR]: storageErrorModal(
    "metadataCheckFailed",
  ),
};
