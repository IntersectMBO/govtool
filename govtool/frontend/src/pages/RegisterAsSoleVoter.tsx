import { useEffect } from "react";
import { Box, Link } from "@mui/material";

import { Background, Typography } from "@atoms";
import { ICONS, PATHS } from "@consts";
import { DashboardTopNav, Footer } from "@organisms";
import { useScreenDimension, useTranslation } from "@hooks";
import { useNavigate } from "react-router-dom";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@/utils/localStorage";
import { SoleVoterInfo } from "@/components/organisms/SoleVoterInfo";

export const RegisterAsSoleVoter = () => {
  const { isMobile, screenWidth } = useScreenDimension();
  const navigate = useNavigate();
  const { t } = useTranslation();

  useEffect(() => {
    if (
      !getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`) ||
      !getItemFromLocalStorage(`${WALLET_LS_KEY}_name`)
    ) {
      navigate(PATHS.home);
    }
  }, []);

  return (
    <Background isReverted>
      <Box display={"flex"} minHeight={"100vh"} flexDirection="column">
        <DashboardTopNav
          imageSRC={ICONS.appLogoIcon}
          imageWidth={isMobile ? undefined : 42}
          imageHeight={isMobile ? 24 : 35}
          title={t("soleVoter.becomeSoleVoter")}
        />

        <Box
          mt={isMobile ? 0 : 7}
          height={isMobile ? "100%" : "auto"}
          sx={{
            marginTop: isMobile ? "97px" : "153px",
            display: isMobile ? "flex" : "grid",
            ...(isMobile && {
              flexDirection: "column",
            }),
            ...(!isMobile && { gridTemplateColumns: "1fr auto 1fr" }),
          }}
        >
          {isMobile && (
            <Box borderBottom={1} borderColor={"#fff"}>
              <Typography
                variant="body2"
                sx={{
                  ml: 2,
                  my: "26px",
                  fontSize: "24px",
                  fontWeight: 400,
                }}
              >
                {t("soleVoter.becomeSoleVoter")}
              </Typography>
            </Box>
          )}
          <Link
            data-testid={"back-to-list-link"}
            sx={{
              cursor: "pointer",
              alignSelf: "flex-start",
              display: "flex",
              justifyContent: "center",
              textDecoration: "none",
              marginBottom: 3,
              mt: isMobile ? 3 : "none",
              mx: isMobile ? 2 : 1,
            }}
            onClick={() => navigate(PATHS.dashboard)}
          >
            <img
              src={ICONS.arrowRightIcon}
              alt="arrow"
              style={{
                marginRight: isMobile
                  ? "12px"
                  : screenWidth < 950
                  ? "8px"
                  : "12px",
                transform: "rotate(180deg)",
              }}
            />
            <Typography
              variant="body2"
              color="primary"
              sx={{
                display: "inline-block",
                fontSize: isMobile
                  ? "14px"
                  : screenWidth < 950
                  ? "12px"
                  : "14px",
              }}
            >
              {t("backToDashboard")}
            </Typography>
          </Link>
          <SoleVoterInfo />
        </Box>
        {isMobile && <Footer />}
      </Box>
    </Background>
  );
};
