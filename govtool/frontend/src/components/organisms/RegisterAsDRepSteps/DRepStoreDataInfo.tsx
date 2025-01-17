import { Dispatch, SetStateAction } from "react";
import { Box, Link } from "@mui/material";

import { Spacer, Typography } from "@atoms";
import { CenteredBoxBottomButtons } from "@molecules";
import {
  useScreenDimension,
  useTranslation,
  useRegisterAsdRepForm,
} from "@hooks";
import { openInNewTab } from "@utils";

import { ControlledField } from "..";
import { LINKS } from "@/consts/links";

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

  const openLink = () => openInNewTab(LINKS.STORING_INFORMATION_OFFLINE);

  return (
    <>
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("registration.storeDataTitle")}
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
        {t("registration.storeDataLink")}
      </Link>
      <ControlledField.Checkbox
        {...{ control, errors }}
        name="storeData"
        label={t("registration.storeDataCheckboxLabel")}
      />
      <Spacer y={isMobile ? 4 : 12.5} />
      <Box display="flex" flex={1} />
      <CenteredBoxBottomButtons
        onActionButton={onClickContinue}
        actionButtonText={t("register")}
        actionButtonDataTestId="register-button"
        disableActionButton={isContinueDisabled}
        onBackButton={onClickBackButton}
      />
    </>
  );
};
