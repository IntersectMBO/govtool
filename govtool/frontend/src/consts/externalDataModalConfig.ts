import { ModalState } from "@/context";
import I18n from "@/i18n";

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
  link: "https://docs.sanchogov.tools",
  linkText: I18n.t("modals.urlCannotBeFound.linkText"),
  buttonText: I18n.t("modals.urlCannotBeFound.buttonText"),
  cancelText: I18n.t("modals.urlCannotBeFound.cancelRegistrationText"),
  feedbackText: I18n.t("modals.urlCannotBeFound.feedbackText"),
};

export const storageInformationErrorModals: Record<
  MetadataHashValidationErrors,
  ModalState<
    typeof externalDataDoesntMatchModal | typeof urlCannotBeFound
  >["state"]
> = {
  [MetadataHashValidationErrors.INVALID_URL]: urlCannotBeFound,
  [MetadataHashValidationErrors.FETCH_ERROR]: urlCannotBeFound,
  [MetadataHashValidationErrors.INVALID_JSON]: externalDataDoesntMatchModal,
  [MetadataHashValidationErrors.INVALID_HASH]: externalDataDoesntMatchModal,
};
