import { Dispatch, SetStateAction, useCallback } from "react";
import { Box, Link } from "@mui/material";

import { Spacer, Typography } from "@atoms";
import {
  useRegisterAsdRepForm,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { openInNewTab } from "@utils";

import { BgCard, ControlledField } from ".";

export const RegisterAsdRepStepThree = ({
  setStep,
}: {
  setStep: Dispatch<SetStateAction<number>>;
}) => {
  const { t } = useTranslation();
  const { control, errors, watch } = useRegisterAsdRepForm();
  const { isMobile } = useScreenDimension();

  const onClickContinue = useCallback(() => setStep(4), [setStep]);

  const onClickBackButton = useCallback(() => setStep(2), [setStep]);

  const isContinueDisabled = !watch("storeData");

  return (
    <BgCard
      actionButtonLabel={t("continue")}
      isActionButtonDisabled={isContinueDisabled}
      onClickActionButton={onClickContinue}
      onClickBackButton={onClickBackButton}
      title={t("registration.becomeADRep")}
    >
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("registration.storeDataTitle")}
      </Typography>
      <Link
        // TODO: Add link about store data when available
        onClick={() => openInNewTab("https://sancho.network/get-started")}
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
