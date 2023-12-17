import { Dispatch, SetStateAction, useMemo } from "react";
import { useNavigate } from "react-router-dom";
import { Box, Link } from "@mui/material";

import { Button, Input, Typography } from "@atoms";
import { PATHS } from "@consts";
import { useScreenDimension, useRegisterAsdRepFormContext } from "@hooks";
import { theme } from "@/theme";
import { openInNewTab } from "@utils";

interface Props {
  setStep: Dispatch<SetStateAction<number>>;
}

export const RegisterAsdRepStepOne = ({ setStep }: Props) => {
  const navigate = useNavigate();
  const {
    palette: { boxShadow2 },
  } = theme;
  const { isMobile, pagePadding, screenWidth } = useScreenDimension();
  const { control, errors, isValid, showSubmitButton } =
    useRegisterAsdRepFormContext();

  const renderCancelButton = useMemo(() => {
    return (
      <Button
        data-testid={"cancel-button"}
        onClick={() => navigate(PATHS.dashboard)}
        size="extraLarge"
        sx={{
          px: 6,
          width: isMobile ? "100%" : "auto",
        }}
        variant="outlined"
      >
        Cancel
      </Button>
    );
  }, [isMobile]);

  const renderConfirmButton = useMemo(() => {
    return (
      <Button
        data-testid={
          showSubmitButton && isValid ? "confirm-button" : "skip-button"
        }
        disabled={!isValid}
        onClick={() => setStep(2)}
        size="extraLarge"
        sx={{
          px: 6,
          width: isMobile ? "100%" : "154px",
        }}
        variant="contained"
      >
        {showSubmitButton ? "Confirm" : "Skip"}
      </Button>
    );
  }, [isMobile, isValid, showSubmitButton]);

  return (
    <Box
      width={screenWidth < 768 ? "auto" : screenWidth < 1024 ? "60vw" : "45vw"}
      boxShadow={isMobile ? "" : `2px 2px 20px 0px ${boxShadow2}`}
      px={pagePadding}
      py={isMobile ? 4 : 8}
      borderRadius={"20px"}
      mb={isMobile ? 0 : 6}
      height={"100%"}
    >
      <Box display="flex" flexDirection="column" alignItems="center">
        <Typography
          color="accentOrange"
          sx={{ letterSpacing: 1.5, textAlign: "center" }}
          variant="body1"
        >
          OPTIONAL
        </Typography>
        <Typography sx={{ mt: 1, textAlign: "center" }} variant="headline4">
          Add Information
        </Typography>
        <Typography
          fontWeight={400}
          sx={{ mb: 7, mt: 3, textAlign: "center" }}
          variant="body1"
        >
          You can include extra information about yourself by adding a URL and
          its hash.
        </Typography>
        <Input
          control={control}
          formFieldName="url"
          placeholder="Your URL with extra info about you"
          dataTestId="url-input"
          errorMessage={errors.url?.message}
          width={isMobile ? "100%" : "70%"}
        />
        <Input
          control={control}
          formFieldName="hash"
          placeholder="The hash of your URL"
          dataTestId="hash-input"
          errorMessage={errors.hash?.message}
          width={isMobile ? "100%" : "70%"}
          marginTop="48px"
        />
        <Link
          data-testid={"how-to-create-link"}
          onClick={() =>
            openInNewTab(
              "https://docs.sanchogov.tools/faqs/how-to-create-a-metadata-anchor"
            )
          }
          alignSelf={"center"}
          mt={5}
          sx={{ cursor: "pointer" }}
        >
          <Typography fontWeight={500} color="primary" variant="body1">
            How to create URL and hash?
          </Typography>
        </Link>
      </Box>
      <Box
        display={"flex"}
        flexDirection={isMobile ? "column" : "row"}
        justifyContent={"space-between"}
        mt={6}
      >
        {isMobile ? renderConfirmButton : renderCancelButton}
        <Box px={2} py={isMobile ? 1.5 : 0} />
        {isMobile ? renderCancelButton : renderConfirmButton}
      </Box>
    </Box>
  );
};
