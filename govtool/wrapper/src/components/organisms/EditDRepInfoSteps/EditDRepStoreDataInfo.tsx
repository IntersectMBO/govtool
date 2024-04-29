import { Dispatch, SetStateAction } from "react";
import { Box, Link } from "@mui/material";

import { Spacer, Typography } from "@atoms";
import {
  useScreenDimension,
  useTranslation,
  useEditDRepInfoForm,
} from "@hooks";
import { openInNewTab } from "@utils";

import { BgCard, ControlledField } from "..";

export const EditDRepStoreDataInfo = ({
  setStep,
}: {
  setStep: Dispatch<SetStateAction<number>>;
}) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const { control, errors, watch } = useEditDRepInfoForm();

  const onClickBackButton = () => setStep(1);

  const onClickContinue = () => setStep(3);

  const isContinueDisabled = !watch("storeData");

  // TODO: Add link about store data when available
  const openLink = () => openInNewTab("https://sancho.network/get-started");

  return (
    <BgCard
      actionButtonLabel={t("register")}
      isActionButtonDisabled={isContinueDisabled}
      onClickActionButton={onClickContinue}
      onClickBackButton={onClickBackButton}
    >
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("editMetadata.storeDataTitle")}
      </Typography>
      <Link
        onClick={openLink}
        sx={{
          cursor: "pointer",
          fontSize: 16,
          fontWeight: 500,
          fontFamily: "Poppins",
          my: 4,
          textAlign: "center",
          textDecoration: "none",
        }}
      >
        {t("editMetadata.storeDataLink")}
      </Link>
      <ControlledField.Checkbox
        {...{ control, errors }}
        name="storeData"
        label={t("editMetadata.storeDataCheckboxLabel")}
      />
      <Spacer y={isMobile ? 4 : 12.5} />
      <Box display="flex" flex={1} />
    </BgCard>
  );
};
