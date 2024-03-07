import { Dispatch, SetStateAction, useCallback, useState } from "react";
import { Box } from "@mui/material";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";

import { Button, Spacer, Typography } from "@atoms";
import { useCreateGovernanceActionForm, useTranslation } from "@hooks";
import { Step } from "@molecules";
import { BgCard, ControlledField } from "@organisms";
import { downloadJson, openInNewTab } from "@utils";

export const StorageInformation = ({
  setStep,
}: {
  setStep: Dispatch<SetStateAction<number>>;
}) => {
  const { t } = useTranslation();
  const {
    control,
    errors,
    createGovernanceAction,
    generateJsonBody,
    getValues,
    watch,
  } = useCreateGovernanceActionForm();
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

  const onClickDowloadJson = () => {
    const data = getValues();
    const jsonBody = generateJsonBody(data);
    downloadJson(jsonBody, fileName);
    setIsJsonDownloaded(true);
  };

  return (
    <BgCard
      actionButtonLabel={t("continue")}
      backButtonLabel={t("back")}
      isActionButtonDisabled={isActionButtonDisabled}
      onClickActionButton={createGovernanceAction}
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
          // TODO: add onClick action when available
          component={
            <Button
              onClick={onClickDowloadJson}
              size="extraLarge"
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
            />
          }
          label={t("createGovernanceAction.storingInformationStep3Label")}
          stepNumber={3}
        />
      </Box>
    </BgCard>
  );
};
