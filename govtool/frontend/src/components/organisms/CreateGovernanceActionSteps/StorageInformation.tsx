import { Dispatch, SetStateAction, useEffect } from "react";
import { Box } from "@mui/material";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";

import { Button, Spacer, Typography } from "@atoms";
import { ICONS } from "@consts";
import {
  useCreateGovernanceActionForm,
  useTranslation,
  useScreenDimension,
} from "@hooks";
import { CenteredBoxBottomButtons, Step } from "@molecules";
import { ControlledField } from "@organisms";
import {
  URL_REGEX,
  ellipsizeText,
  isValidURLLength,
  openInNewTab,
} from "@utils";

type StorageInformationProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

export const StorageInformation = ({ setStep }: StorageInformationProps) => {
  const { t } = useTranslation();
  const {
    control,
    errors,
    createGovernanceAction,
    getValues,
    watch,
    generateMetadata,
    onClickDownloadJson,
    isLoading,
  } = useCreateGovernanceActionForm(setStep);
  const { screenWidth } = useScreenDimension();

  const fileName = getValues("governance_action_type") as string;

  const openGuideAboutStoringInformation = () =>
    openInNewTab("https://docs.gov.tools/faqs/how-to-create-a-metadata-anchor");

  const isActionButtonDisabled = !watch("storingURL") || !!errors.storingURL;

  const onClickBack = () => setStep(5);

  useEffect(() => {
    generateMetadata();
  }, []);

  return (
    <>
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("createGovernanceAction.storingInformationTitle")}
      </Typography>
      <Button
        data-testid="read-guide-button"
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
        sx={{ alignSelf: "center", width: "fit-content" }}
        variant="text"
      >
        {t("createGovernanceAction.storingInformationStep2Link")}
      </Button>
      <Typography fontWeight={400} sx={{ textAlign: "center" }} variant="body1">
        {t("createGovernanceAction.storingInformationDescription")}
      </Typography>
      <Box sx={{ my: 4 }}>
        <Step
          component={
            <Button
              data-testid="metadata-download-button"
              onClick={onClickDownloadJson}
              size="extraLarge"
              startIcon={<img alt="download" src={ICONS.download} />}
              sx={{
                width: "fit-content",
                ml: screenWidth < 1024 ? 0 : 1.75,
                mt: screenWidth < 1024 ? 1.5 : 0,
              }}
              variant="outlined"
            >
              {`${ellipsizeText(fileName, 8)}.jsonld`}
            </Button>
          }
          componentsLayoutStyles={{
            alignItems: screenWidth < 1024 ? undefined : "center",
            flexDirection: screenWidth < 1024 ? "column" : "row",
          }}
          label={t("createGovernanceAction.storingInformationStep1Label")}
          stepNumber={1}
        />
        <Spacer y={6} />
        <Step
          label={t("createGovernanceAction.storingInformationStep2Label")}
          stepNumber={2}
        />
        <Spacer y={6} />
        <Step
          component={
            <ControlledField.Input
              {...{ control, errors }}
              dataTestId="metadata-url-input"
              name="storingURL"
              layoutStyles={{ mt: 1.5 }}
              placeholder={t(
                "createGovernanceAction.storingInformationURLPlaceholder",
              )}
              rules={{
                required: {
                  value: true,
                  message: t(
                    "createGovernanceAction.fields.validations.required",
                  ),
                },
                pattern: {
                  value: URL_REGEX,
                  message: t("createGovernanceAction.fields.validations.url"),
                },
                validate: isValidURLLength,
              }}
            />
          }
          label={t("createGovernanceAction.storingInformationStep3Label")}
          stepNumber={3}
        />
      </Box>
      <CenteredBoxBottomButtons
        onActionButton={createGovernanceAction}
        disableActionButton={isActionButtonDisabled}
        isLoadingActionButton={isLoading}
        onBackButton={onClickBack}
        backButtonText={t("cancel")}
      />
    </>
  );
};
