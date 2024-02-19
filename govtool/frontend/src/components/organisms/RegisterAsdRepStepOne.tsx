import { Dispatch, SetStateAction, useMemo } from "react";
import { useNavigate } from "react-router-dom";
import { Box, Link } from "@mui/material";

import { Button, Spacer, Typography } from "@atoms";
import { PATHS } from "@consts";
import {
  useScreenDimension,
  useRegisterAsdRepFormContext,
  useTranslation,
} from "@hooks";
import { theme } from "@/theme";
import { openInNewTab } from "@utils";

import { ControlledField } from ".";

interface Props {
  setStep: Dispatch<SetStateAction<number>>;
}

export const RegisterAsdRepStepOne = ({ setStep }: Props) => {
  const navigate = useNavigate();
  const { t } = useTranslation();
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
        {t("cancel")}
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
        {showSubmitButton ? t("confirm") : t("skip")}
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
          {t("registration.optional")}
        </Typography>
        <Typography sx={{ mt: 1, textAlign: "center" }} variant="headline4">
          {t("registration.headingStepOne")}
        </Typography>
        <Typography
          fontWeight={400}
          sx={{ mb: 7, mt: 3, textAlign: "center" }}
          variant="body1"
        >
          {t("registration.descriptionStepOne")}
        </Typography>
        <ControlledField.Input
          {...{ control, errors }}
          dataTestId="url-input"
          layoutStyles={{ width: isMobile ? "100%" : "70%" }}
          name="url"
          placeholder={t("forms.urlWithInfoPlaceholder")}
        />
        <Spacer y={6} />
        <ControlledField.Input
          {...{ control, errors }}
          dataTestId="hash-input"
          layoutStyles={{ width: isMobile ? "100%" : "70%" }}
          name="hash"
          placeholder={t("forms.hashPlaceholder")}
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
            {t("forms.howCreateUrlAndHash")}
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
