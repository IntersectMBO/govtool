import { Dispatch, SetStateAction, useMemo } from "react";
import { Box } from "@mui/material";

import { LoadingButton, Button, Typography } from "@atoms";
import { theme } from "@/theme";
import { useRegisterAsdRepFormContext, useScreenDimension } from "@hooks";
import { usei18n } from "@translations";

interface Props {
  setStep: Dispatch<SetStateAction<number>>;
}

export const RegisterAsdRepStepTwo = ({ setStep }: Props) => {
  const {
    palette: { boxShadow2 },
  } = theme;
  const { isLoading, submitForm } = useRegisterAsdRepFormContext();
  const { isMobile, pagePadding, screenWidth } = useScreenDimension();
  const { t } = usei18n();

  const renderBackButton = useMemo(() => {
    return (
      <Button
        data-testid={"back-button"}
        onClick={() => setStep(1)}
        size="extraLarge"
        sx={{
          px: 6,
          width: isMobile ? "100%" : "auto",
        }}
        variant="outlined"
      >
        {t("back")}
      </Button>
    );
  }, [isMobile]);

  const renderRegisterButton = useMemo(() => {
    return (
      <LoadingButton
        data-testid={"register-button"}
        isLoading={isLoading}
        onClick={submitForm}
        sx={{
          borderRadius: 50,
          textTransform: "none",
          px: 6,
          width: isMobile ? "100%" : "auto",
          height: 48,
        }}
        variant="contained"
      >
        {t("registration.register")}
      </LoadingButton>
    );
  }, [isLoading, isMobile, submitForm]);

  return (
    <Box
      width={screenWidth < 768 ? "auto" : screenWidth < 1024 ? "60vw" : "45vw"}
      boxShadow={isMobile ? "" : `2px 2px 20px 0px ${boxShadow2}`}
      px={pagePadding}
      py={isMobile ? 4 : 8}
      borderRadius={"20px"}
      mb={isMobile ? 0 : 6}
      height="100%"
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
      <Box
        display="flex"
        flexDirection={isMobile ? "column" : "row"}
        justifyContent="space-between"
        mt={6}
      >
        {isMobile ? renderRegisterButton : renderBackButton}
        <Box px={2} py={isMobile ? 1.5 : 0} />
        {isMobile ? renderBackButton : renderRegisterButton}
      </Box>
    </Box>
  );
};
