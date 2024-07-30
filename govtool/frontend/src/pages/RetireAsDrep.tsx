import { useEffect } from "react";
import { useNavigate } from "react-router-dom";
import { Box } from "@mui/material";

import { Background } from "@atoms";
import { PATHS } from "@consts";
import { DashboardTopNav, Footer, WhatRetirementMeans } from "@organisms";
import { useScreenDimension, useTranslation } from "@hooks";
import { LinkWithIcon } from "@molecules";
import { checkIsWalletConnected } from "@utils";

export const RetireAsDrep = () => {
  const navigate = useNavigate();
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();

  useEffect(() => {
    if (!checkIsWalletConnected()) {
      navigate(PATHS.home);
    }
  }, []);

  const onClickBackToDashboard = () => navigate(PATHS.dashboard);

  return (
    <Background isReverted>
      <Box
        sx={{ display: "flex", flexDirection: "column", minHeight: "100vh" }}
      >
        <DashboardTopNav
          isVotingPowerHidden
          title={t("retirement.retireAsDrep")}
        />
        <LinkWithIcon
          label={t("backToDashboard")}
          onClick={onClickBackToDashboard}
          sx={{
            mb: isMobile ? 0 : 1.5,
            ml: isMobile ? 2 : 5,
            mt: isMobile ? 3 : 1.5,
          }}
        />
        <WhatRetirementMeans onClickCancel={onClickBackToDashboard} />
        {/* FIXME: Footer should be on top of the layout.
        Should not be rerendered across the pages */}
        <Footer />
      </Box>
    </Background>
  );
};
