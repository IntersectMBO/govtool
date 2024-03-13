import { Dispatch, SetStateAction, useCallback } from "react";
import { Box } from "@mui/material";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";

import { Button, Spacer, Typography } from "@atoms";
import { ICONS } from "@consts";
import { useCreateGovernanceActionForm, useTranslation } from "@hooks";
import { Step } from "@molecules";
import { BgCard, ControlledField } from "@organisms";
import { URL_REGEX, openInNewTab } from "@utils";

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
    onClickDownloadJson,
    isLoading,
  } = useCreateGovernanceActionForm(setStep);

  // TODO: change on correct file name
  const fileName = getValues("governance_action_type");

  // TODO: Change link to correct
  const openGuideAboutStoringInformation = useCallback(
    () => openInNewTab("https://sancho.network/"),
    []
  );

  const isActionButtonDisabled = !watch("storingURL");

  const onClickBack = useCallback(() => setStep(5), []);

  return (
    <BgCard
      actionButtonLabel={t("continue")}
      backButtonLabel={t("back")}
      isActionButtonDisabled={isActionButtonDisabled}
      onClickActionButton={createGovernanceAction}
      onClickBackButton={onClickBack}
      isLoadingActionButton={isLoading}
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
