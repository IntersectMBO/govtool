import { Dispatch, SetStateAction, useCallback } from "react";
import { Box } from "@mui/material";

import { Typography } from "@atoms";
import {
  useRegisterAsdRepFormContext,
  useScreenDimension,
  useTranslation,
} from "@hooks";

import { BgCard } from ".";

interface Props {
  setStep: Dispatch<SetStateAction<number>>;
}

export const RegisterAsdRepStepThree = ({ setStep }: Props) => {
  const { isLoading, submitForm } = useRegisterAsdRepFormContext();
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  const onClickBackButton = useCallback(() => setStep(2), []);

  return (
    <BgCard
      actionButtonLabel={t("registration.register")}
      isLoadingActionButton={isLoading}
      onClickActionButton={submitForm}
      onClickBackButton={onClickBackButton}
    >
      <Box display="flex" flexDirection="column">
        <Typography sx={{ mt: 1, textAlign: "center" }} variant="headline4">
          {t("registration.headingStepTwo")}
        </Typography>
        <Typography
          fontWeight={400}
          sx={{
            mb: 7,
            mt: isMobile ? 4 : 10,
            textAlign: "center",
            whiteSpace: "pre-line",
          }}
          variant="body1"
        >
          {t("registration.descriptionStepTwo")}
        </Typography>
      </Box>
    </BgCard>
  );
};
