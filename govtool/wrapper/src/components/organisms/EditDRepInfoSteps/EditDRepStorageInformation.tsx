import { Dispatch, SetStateAction, useEffect } from "react";
import { Box } from "@mui/material";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";

import { Button, Spacer, Typography } from "@atoms";
import { ICONS, Rules } from "@consts";
import {
  useEditDRepInfoForm,
  useTranslation,
  useScreenDimension,
} from "@hooks";
import { Step } from "@molecules";
import { BgCard, ControlledField } from "@organisms";
import { openInNewTab } from "@utils";

type StorageInformationProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

export const EditDRepStorageInformation = ({
  setStep,
}: StorageInformationProps) => {
  const { t } = useTranslation();
  const {
    control,
    errors,
    generateMetadata,
    getValues,
    isEditDRepMetadataLoading,
    onClickDownloadJson,
    editDRepInfo,
    watch,
  } = useEditDRepInfoForm();
  const { screenWidth } = useScreenDimension();

  const fileName = getValues("dRepName");

  const openGuideAboutStoringInformation = () =>
    openInNewTab(
      "https://docs.sanchogov.tools/faqs/how-to-create-a-metadata-anchor",
    );

  const isActionButtonDisabled = !watch("storingURL") || !!errors.storingURL;

  const onClickBack = () => setStep(2);

  useEffect(() => {
    generateMetadata();
  }, []);

  return (
    <BgCard
      actionButtonLabel={t("submit")}
      backButtonLabel={t("back")}
      isActionButtonDisabled={isActionButtonDisabled}
      onClickActionButton={editDRepInfo}
      onClickBackButton={onClickBack}
      isLoadingActionButton={isEditDRepMetadataLoading}
    >
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("editMetadata.storingInformationTitle")}
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
        {t("editMetadata.storingInformationStep2Link")}
      </Button>
      <Typography fontWeight={400} sx={{ textAlign: "center" }} variant="body1">
        {t("editMetadata.storingInformationDescription")}
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
              {`${fileName}.jsonld`}
            </Button>
          }
          label={t("editMetadata.storingInformationStep1Label")}
          componentsLayoutStyles={{
            alignItems: screenWidth < 1024 ? undefined : "center",
            flexDirection: screenWidth < 1024 ? "column" : "row",
          }}
          stepNumber={1}
        />
        <Spacer y={6} />
        <Step
          layoutStyles={{ alignItems: "center" }}
          label={t("editMetadata.storingInformationStep2Label")}
          stepNumber={2}
        />
        <Spacer y={6} />
        <Step
          component={
            <ControlledField.Input
              {...{ control, errors }}
              layoutStyles={{ mt: 1.5 }}
              name="storingURL"
              placeholder={t("editMetadata.storingInformationURLPlaceholder")}
              rules={Rules.STORING_LINK}
            />
          }
          label={t("editMetadata.storingInformationStep3Label")}
          stepNumber={3}
        />
      </Box>
    </BgCard>
  );
};
