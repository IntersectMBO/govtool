import { Dispatch, SetStateAction } from "react";
import { Box, Link } from "@mui/material";

import { Spacer, Typography } from "@atoms";
import {
  useScreenDimension,
  useTranslation,
  useRegisterAsdRepForm,
} from "@hooks";
import { openInNewTab } from "@utils";

import { BgCard, ControlledField } from "..";

export const DRepStoreDataInfo = ({
  setStep,
}: {
  setStep: Dispatch<SetStateAction<number>>;
}) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const { control, errors, watch } = useRegisterAsdRepForm();

  const onClickBackButton = () => setStep(2);

  const isContinueDisabled = !watch("storeData");

  const onClickContinue = () => setStep(4);

  // TODO: Add link about store data when available
  const link = "https://sancho.network/get-started";

  return (
    <BgCard
      actionButtonLabel={t("register")}
      isActionButtonDisabled={isContinueDisabled}
      onClickActionButton={onClickContinue}
      onClickBackButton={onClickBackButton}
    >
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("registration.storeDataTitle")}
      </Typography>
      <Link
        href={link}
        target="_blank"
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
        {t("registration.storeDataLink")}
      </Link>
      <ControlledField.Checkbox
        {...{ control, errors }}
        name="storeData"
        label={t("registration.storeDataCheckboxLabel")}
      />
      <Spacer y={isMobile ? 4 : 12.5} />
      <Box display="flex" flex={1} />
    </BgCard>
  );
};
