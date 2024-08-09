import { Dispatch, SetStateAction, useEffect } from "react";
import { Box } from "@mui/material";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";

import { Button, Spacer, Typography } from "@atoms";
import { ICONS, Rules } from "@consts";
import {
  useRegisterAsdRepForm,
  useTranslation,
  useScreenDimension,
} from "@hooks";
import { CenteredBoxBottomButtons, Step } from "@molecules";
import { ControlledField } from "@organisms";
import { openInNewTab, ellipsizeText } from "@utils";

type StorageInformationProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

export const DRepStorageInformation = ({
  setStep,
}: StorageInformationProps) => {
  const { t } = useTranslation();
  const {
    control,
    errors,
    generateMetadata,
    getValues,
    isRegistrationAsDRepLoading,
    onClickDownloadJson,
    registerAsDrep,
    watch,
  } = useRegisterAsdRepForm(setStep);
  const { screenWidth } = useScreenDimension();

  const fileName = getValues("dRepName");

  const openGuideAboutStoringInformation = () =>
    openInNewTab(
      "https://docs.sanchogov.tools/faqs/how-to-create-a-metadata-anchor",
    );

  const isActionButtonDisabled = !watch("storingURL") || !!errors.storingURL;

  const onClickBack = () => setStep(3);

  useEffect(() => {
    generateMetadata();
  }, []);

  return (
    <>
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("registration.storingInformationTitle")}
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
        {t("registration.storingInformationStep2Link")}
      </Button>
      <Typography fontWeight={400} sx={{ textAlign: "center" }} variant="body1">
        {t("registration.storingInformationDescription")}
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
          label={t("registration.storingInformationStep1Label")}
          componentsLayoutStyles={{
            alignItems: screenWidth < 1024 ? undefined : "center",
            flexDirection: screenWidth < 1024 ? "column" : "row",
          }}
          stepNumber={1}
        />
        <Spacer y={6} />
        <Step
          layoutStyles={{ alignItems: "center" }}
          label={t("registration.storingInformationStep2Label")}
          stepNumber={2}
        />
        <Spacer y={6} />
        <Step
          component={
            <ControlledField.Input
              {...{ control, errors }}
              dataTestId="metadata-url-input"
              layoutStyles={{ mt: 1.5 }}
              name="storingURL"
              placeholder={t("registration.storingInformationURLPlaceholder")}
              rules={Rules.STORING_LINK}
            />
          }
          label={t("registration.storingInformationStep3Label")}
          stepNumber={3}
        />
      </Box>
      <CenteredBoxBottomButtons
        onActionButton={registerAsDrep}
        actionButtonText={t("submit")}
        actionButtonDataTestId="submit-button"
        disableActionButton={isActionButtonDisabled}
        isLoadingActionButton={isRegistrationAsDRepLoading}
        onBackButton={onClickBack}
      />
    </>
  );
};
