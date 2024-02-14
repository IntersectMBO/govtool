import { useMemo } from "react";
import { Box, Link } from "@mui/material";

import { LoadingButton, Button, Typography } from "@atoms";
import { theme } from "@/theme";
import { useScreenDimension, useTranslation } from "@hooks";
import { Trans } from "react-i18next";
import { openInNewTab } from "@/utils";
import { useNavigate } from "react-router-dom";
import { PATHS } from "@consts";

export const SoleVoterInfo = () => {
  const {
    palette: { boxShadow2 },
  } = theme;
  const { isMobile, pagePadding, screenWidth } = useScreenDimension();
  const navigate = useNavigate();
  const { t } = useTranslation();

  const renderBackButton = useMemo(() => {
    return (
      <Button
        data-testid={"back-button"}
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

  const renderRegisterButton = useMemo(() => {
    return (
      <LoadingButton
        data-testid={"register-button"}
        // isLoading={isLoading}
        onClick={() => {}}
        sx={{
          borderRadius: 50,
          textTransform: "none",
          px: 6,
          width: isMobile ? "100%" : "auto",
          height: 48,
        }}
        variant="contained"
      >
        {t("soleVoter.continueToRegister")}
      </LoadingButton>
    );
  }, [
    // isLoading,
    isMobile,
  ]);

  return (
    <Box
      width={screenWidth < 768 ? "auto" : screenWidth < 1024 ? "60vw" : "45vw"}
      boxShadow={isMobile ? "" : `2px 2px 20px 0px ${boxShadow2}`}
      px={pagePadding}
      py={isMobile ? 3 : 8}
      borderRadius={"20px"}
      mb={isMobile ? 0 : 6}
      height="auto"
    >
      <Box display="flex" flexDirection="column">
        <Typography sx={{ mt: 1, textAlign: "center" }} variant="headline4">
          {t("soleVoter.heading")}
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
          <Trans
            i18nKey="soleVoter.description"
            components={[
              <Link
                onClick={() => openInNewTab("https://sancho.network/")}
                sx={{ cursor: "pointer" }}
                key="0"
              />,
            ]}
          />
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
