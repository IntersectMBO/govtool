import { useCallback, useEffect, useMemo } from "react";
import { useNavigate } from "react-router-dom";
import { Trans } from "react-i18next";
import { Box, Link } from "@mui/material";
import ArrowBackIosIcon from "@mui/icons-material/ArrowBackIos";

import { Background, Button, Typography } from "@atoms";
import { useCardano } from "@context";
import { ICONS, PATHS } from "@consts";
import { useScreenDimension, useTranslation } from "@hooks";
import { DashboardTopNav, Footer } from "@organisms";
import {
  WALLET_LS_KEY,
  correctAdaFormat,
  getItemFromLocalStorage,
  openInNewTab,
} from "@utils";
import { theme } from "@/theme";

export const RetireAsSoleVoter = () => {
  const {
    palette: { boxShadow2 },
  } = theme;
  const { isMobile, screenWidth } = useScreenDimension();
  const navigate = useNavigate();
  const { t } = useTranslation();
  const { soleVoter } = useCardano();

  const navigateToDashboard = useCallback(
    () => navigate(PATHS.dashboard),
    [navigate]
  );

  useEffect(() => {
    if (
      !getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`) ||
      !getItemFromLocalStorage(`${WALLET_LS_KEY}_name`) ||
      !soleVoter?.isRegistered
    ) {
      navigate(PATHS.home);
    }
  }, []);

  const renderBackButton = useMemo(() => {
    return (
      <Button
        data-testid="back-button"
        onClick={navigateToDashboard}
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
  }, [isMobile, navigateToDashboard]);

  const renderRegisterButton = useMemo(() => {
    return (
      <Button
        data-testid="retire-button"
        onClick={() => {}}
        size="extraLarge"
        sx={{
          px: 6,
          width: isMobile ? "100%" : "auto",
        }}
        variant="contained"
      >
        {t("continue")}
      </Button>
    );
  }, [isMobile]);

  return (
    <Background isReverted>
      <Box display="flex" flexDirection="column" minHeight="100vh">
        <DashboardTopNav
          imageHeight={isMobile ? 24 : 35}
          imageSRC={ICONS.appLogoIcon}
          imageWidth={isMobile ? undefined : 42}
          title={t("soleVoter.retireSoleVoter")}
        />

        <Box
          height={isMobile ? "100%" : "auto"}
          mt={isMobile ? 0 : 7}
          sx={{
            alignItems: screenWidth >= 768 ? "center" : "inherit",
            marginTop: screenWidth < 1440 ? "97px" : "153px",
            display: screenWidth < 1440 ? "flex" : "grid",
            ...(screenWidth < 1440 && {
              flexDirection: "column",
            }),
            ...(screenWidth >= 1440 && { gridTemplateColumns: "1fr auto 1fr" }),
          }}
        >
          {isMobile && (
            <Box borderColor="white" borderBottom={1}>
              <Typography
                variant="title1"
                sx={{
                  ml: 2,
                  my: 3.25,
                }}
              >
                {t("soleVoter.retireSoleVoter")}
              </Typography>
            </Box>
          )}
          <Link
            data-testid="back-to-list-link"
            sx={{
              alignItems: "center",
              alignSelf: "flex-start",
              cursor: "pointer",
              display: "flex",
              justifyContent: "flex-start",
              marginBottom: 3,
              ml: screenWidth < 1440 ? 2 : 5,
              mt: screenWidth < 1440 ? 3 : "none",
              textDecoration: "none",
            }}
            onClick={navigateToDashboard}
          >
            <ArrowBackIosIcon sx={{ fontSize: 14 }} />
            <Typography color="primary" fontWeight={400} variant="body2">
              {t("backToDashboard")}
            </Typography>
          </Link>
          <Box
            borderRadius="20px"
            boxShadow={isMobile ? "" : `2px 2px 20px 0px ${boxShadow2}`}
            height="auto"
            maxWidth={screenWidth > 768 ? 600 : undefined}
            px={isMobile ? 2 : 18.75}
            py={isMobile ? 6.25 : 10}
          >
            <Box display="flex" flexDirection="column">
              <Typography sx={{ textAlign: "center" }} variant="headline4">
                {t("soleVoter.retirementHeading")}
              </Typography>
              <Typography
                fontWeight={400}
                sx={{ py: 5, textAlign: "center", whiteSpace: "pre-line" }}
                variant="body1"
              >
                <Trans
                  components={[
                    <Typography fontWeight={500} key={0} variant="body1" />,
                    <Link
                      key="1"
                      onClick={() => openInNewTab("https://sancho.network/")}
                      sx={{ cursor: "pointer" }}
                    />,
                  ]}
                  i18nKey={"soleVoter.retirementDescription"}
                  values={{ deposit: correctAdaFormat(soleVoter?.deposit) }}
                />
              </Typography>
            </Box>
            <Box
              display="flex"
              flexDirection={isMobile ? "column" : "row"}
              justifyContent="space-between"
            >
              {isMobile ? renderRegisterButton : renderBackButton}
              <Box px={2} py={isMobile ? 1.5 : 0} />
              {isMobile ? renderBackButton : renderRegisterButton}
            </Box>
          </Box>
          {isMobile && <Footer />}
        </Box>
      </Box>
    </Background>
  );
};
