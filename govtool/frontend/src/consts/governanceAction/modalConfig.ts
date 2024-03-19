import { ModalState } from "@/context";
import I18n from "@/i18n";

import { MetadataHashValidationErrors } from "./metadataHashValidationErrors";

const externalDataDoesntMatchModal = {
  status: "warning",
  title: I18n.t("createGovernanceAction.modals.externalDataDoesntMatch.title"),
  message: I18n.t(
    "createGovernanceAction.modals.externalDataDoesntMatch.message",
  ),
  buttonText: I18n.t(
    "createGovernanceAction.modals.externalDataDoesntMatch.buttonText",
  ),
  cancelText: I18n.t(
    "createGovernanceAction.modals.externalDataDoesntMatch.cancelRegistrationText",
  ),
  feedbackText: I18n.t(
    "createGovernanceAction.modals.externalDataDoesntMatch.feedbackText",
  ),
} as const;

const urlCannotBeFound = {
  status: "warning",
  title: I18n.t("createGovernanceAction.modals.urlCannotBeFound.title"),
  message: I18n.t("createGovernanceAction.modals.urlCannotBeFound.message"),
  link: "https://docs.sanchogov.tools",
  linkText: I18n.t("createGovernanceAction.modals.urlCannotBeFound.linkText"),
  buttonText: I18n.t(
    "createGovernanceAction.modals.urlCannotBeFound.buttonText",
  ),
  cancelText: I18n.t(
    "createGovernanceAction.modals.urlCannotBeFound.cancelRegistrationText",
  ),
  feedbackText: I18n.t(
    "createGovernanceAction.modals.urlCannotBeFound.feedbackText",
  ),
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
