import { Dispatch, SetStateAction, useCallback, useState } from "react";
import { useNavigate } from "react-router-dom";
import { Box } from "@mui/material";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";

import { Button, Spacer, Typography } from "@atoms";
import { ICONS, PATHS } from "@consts";
import { useCreateGovernanceActionForm, useTranslation } from "@hooks";
import { Step } from "@molecules";
import { BgCard, ControlledField, StatusModalState } from "@organisms";
import {
  URL_REGEX,
  downloadJson,
  openInNewTab,
  validateMetadataHash,
  MetadataHashValidationErrors,
} from "@utils";
import { ModalState, useModal } from "@/context";
import I18n from "@/i18n";

type StorageInformationProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

const externalDataDoesntMatchModal = {
  type: "statusModal",
  state: {
    status: "warning",
    title: I18n.t(
      "createGovernanceAction.modals.externalDataDoesntMatch.title"
    ),
    message: I18n.t(
      "createGovernanceAction.modals.externalDataDoesntMatch.message"
    ),
    buttonText: I18n.t(
      "createGovernanceAction.modals.externalDataDoesntMatch.buttonText"
    ),
    cancelText: I18n.t(
      "createGovernanceAction.modals.externalDataDoesntMatch.cancelRegistrationText"
    ),
    feedbackText: I18n.t(
      "createGovernanceAction.modals.externalDataDoesntMatch.feedbackText"
    ),
  },
} as const;

const urlCannotBeFound = {
  type: "statusModal",
  state: {
    status: "warning",
    title: I18n.t("createGovernanceAction.modals.urlCannotBeFound.title"),
    message: I18n.t("createGovernanceAction.modals.urlCannotBeFound.message"),
    link: "https://docs.sanchogov.tools",
    linkText: I18n.t("createGovernanceAction.modals.urlCannotBeFound.linkText"),
    buttonText: I18n.t(
      "createGovernanceAction.modals.urlCannotBeFound.buttonText"
    ),
    cancelText: I18n.t(
      "createGovernanceAction.modals.urlCannotBeFound.cancelRegistrationText"
    ),
    feedbackText: I18n.t(
      "createGovernanceAction.modals.urlCannotBeFound.feedbackText"
    ),
  },
} as const;

const storageInformationErrorModals: Record<
  MetadataHashValidationErrors,
  ModalState<
    | (typeof externalDataDoesntMatchModal)["state"]
    | (typeof urlCannotBeFound)["state"]
  >
> = {
  [MetadataHashValidationErrors.INVALID_URL]: urlCannotBeFound,
  [MetadataHashValidationErrors.FETCH_ERROR]: urlCannotBeFound,
  [MetadataHashValidationErrors.INVALID_JSON]: externalDataDoesntMatchModal,
  [MetadataHashValidationErrors.INVALID_HASH]: externalDataDoesntMatchModal,
};

export const StorageInformation = ({ setStep }: StorageInformationProps) => {
  const { t } = useTranslation();
  const navigate = useNavigate();
  const {
    control,
    errors,
    createGovernanceAction,
    generateJsonBody,
    getValues,
    watch,
  } = useCreateGovernanceActionForm();
  const { openModal, closeModal } = useModal();
  const [isJsonDownloaded, setIsJsonDownloaded] = useState<boolean>(false);

  // TODO: change on correct file name
  const fileName = getValues("governance_action_type");

  // TODO: Change link to correct
  const openGuideAboutStoringInformation = useCallback(
    () => openInNewTab("https://sancho.network/"),
    []
  );

  const isActionButtonDisabled = !watch("storingURL") || !isJsonDownloaded;

  const onClickBack = useCallback(() => setStep(5), []);

  const onClickDownloadJson = async () => {
    const data = getValues();
    const jsonBody = await generateJsonBody(data);
    downloadJson(jsonBody, fileName);
    setIsJsonDownloaded(true);
  };

  const backToDashboard = () => {
    navigate(PATHS.dashboard);
    closeModal();
  };

  const handleStoringURLJSONValidation = useCallback(async () => {
    const storingURL = getValues("storingURL");
    try {
      await createGovernanceAction();

      // TODO: To be replaced wtih the correct hash
      await validateMetadataHash(storingURL, "hash");
    } catch (error: any) {
      if (Object.values(MetadataHashValidationErrors).includes(error.message)) {
        openModal({
          ...storageInformationErrorModals[
            error.message as MetadataHashValidationErrors
          ],
          onSubmit: () => {
            setStep(3);
          },
          onCancel: backToDashboard,
          // TODO: Open usersnap feedback
          onFeedback: backToDashboard,
        } as ModalState<StatusModalState>);
      }
    }
  }, [getValues, generateJsonBody]);

  return (
    <BgCard
      actionButtonLabel={t("continue")}
      backButtonLabel={t("back")}
      isActionButtonDisabled={isActionButtonDisabled}
      onClickActionButton={handleStoringURLJSONValidation}
      onClickBackButton={onClickBack}
    >
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("createGovernanceAction.storingInformationTitle")}
      </Typography>
      <Typography
        fontWeight={400}
        sx={{ mt: 1, textAlign: "center" }}
        variant="body1"
      >
        {t("createGovernanceAction.storingInformationDescription")}
      </Typography>
      <Box sx={{ my: 4 }}>
        <Step
          component={
            <Button
              onClick={onClickDownloadJson}
              size="extraLarge"
              startIcon={<img src={ICONS.download} />}
              sx={{ width: "fit-content" }}
            >
              {`${fileName}.jsonld`}
            </Button>
          }
          label={t("createGovernanceAction.storingInformationStep1Label")}
          stepNumber={1}
        />
        <Spacer y={6} />
        <Step
          component={
            <Button
              endIcon={
                <OpenInNewIcon
                  sx={{
                    color: "primary",
                    height: 17,
                    width: 17,
                  }}
                />
              }
              onClick={openGuideAboutStoringInformation}
              size="extraLarge"
              sx={{ width: "fit-content" }}
              variant="text"
            >
              {t("createGovernanceAction.storingInformationStep2Link")}
            </Button>
          }
          label={t("createGovernanceAction.storingInformationStep2Label")}
          stepNumber={2}
        />
        <Spacer y={6} />
        <Step
          component={
            <ControlledField.Input
              {...{ control, errors }}
              name="storingURL"
              placeholder={t(
                "createGovernanceAction.storingInformationURLPlaceholder"
              )}
              rules={{
                required: {
                  value: true,
                  message: t(
                    "createGovernanceAction.fields.validations.required"
                  ),
                },
                pattern: {
                  value: URL_REGEX,
                  message: t("createGovernanceAction.fields.validations.url"),
                },
              }}
            />
          }
          label={t("createGovernanceAction.storingInformationStep3Label")}
          stepNumber={3}
        />
      </Box>
    </BgCard>
  );
};
