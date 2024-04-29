import { Dispatch, SetStateAction, useEffect } from "react";
import { Box } from "@mui/material";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";

import { Button, Spacer, Typography } from "@atoms";
import { ICONS } from "@consts";
import { useTranslation, useScreenDimension, useVoteContextForm } from "@hooks";
import { Step } from "@molecules";
import { ControlledField, VoteContextWrapper } from "@organisms";
import { URL_REGEX, openInNewTab } from "@utils";

type VoteContextStoringInformationProps = {
  setStep: Dispatch<SetStateAction<number>>;
  setSavedHash: Dispatch<SetStateAction<string | null>>;
  setErrorMessage: Dispatch<SetStateAction<string | undefined>>;
  onCancel: () => void;
};

export const VoteContextStoringInformation = ({
  setStep,
  setSavedHash,
  setErrorMessage,
  onCancel,
}: VoteContextStoringInformationProps) => {
  const { t } = useTranslation();
  const { screenWidth } = useScreenDimension();

  const {
    control,
    errors,
    validateURL,
    watch,
    generateMetadata,
    onClickDownloadJson,
  } = useVoteContextForm(setSavedHash, setStep, setErrorMessage);

  const openGuideAboutStoringInformation = () =>
    openInNewTab(
      "https://docs.sanchogov.tools/faqs/how-to-create-a-metadata-anchor",
    );

  const isContinueDisabled = !watch("storingURL");

  useEffect(() => {
    generateMetadata();
  }, []);

  return (
    <VoteContextWrapper
      onContinue={validateURL}
      isContinueDisabled={isContinueDisabled}
      onCancel={onCancel}
    >
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("createGovernanceAction.storingInformationTitle")}
      </Typography>
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
              {t("govActions.voteContextJsonldFileName")}
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
              }}
            />
          }
          label={t("createGovernanceAction.storingInformationStep3Label")}
          stepNumber={3}
        />
      </Box>
    </VoteContextWrapper>
  );
};
