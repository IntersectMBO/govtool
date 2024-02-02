import { Dispatch, SetStateAction, useMemo } from "react";
import { Box } from "@mui/material";

import { LoadingButton, Button, Typography } from "@atoms";
import { theme } from "@/theme";
import { useRegisterAsdRepFormContext, useScreenDimension } from "@hooks";

interface Props {
  setStep: Dispatch<SetStateAction<number>>;
}

export const RegisterAsdRepStepTwo = ({ setStep }: Props) => {
  const {
    palette: { boxShadow2 },
  } = theme;
  const { isLoading, submitForm } = useRegisterAsdRepFormContext();
  const { isMobile, pagePadding, screenWidth } = useScreenDimension();

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
        Back
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
        Register
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
          Confirm DRep registration
        </Typography>
        <Typography
          fontWeight={400}
          sx={{ mb: 7, mt: isMobile ? 4 : 10, textAlign: "center" }}
          variant="body1"
        >
          By clicking register you create your DRep ID within your wallet and
          become a DRep. <br />
          <br />
          Once the registration has completed your DRep ID will be shown on your
          dashboard. You will be able to share your DRep ID so that other ada
          holders can delegate their voting power to you.
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
