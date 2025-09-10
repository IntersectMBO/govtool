import { useEffect } from "react";
import { useNavigate } from "react-router-dom";
import { Box, Divider } from "@mui/material";

import { Background, ScrollToManage, Typography } from "@atoms";
import {
  GOVERNANCE_ACTIONS_FILTERS,
  GOVERNANCE_ACTIONS_SORTING,
  PATHS,
} from "@consts";
import { useCardano, useDataActionsBar } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";
import { DataActionsBar } from "@molecules";
import { Footer, TopNav, GovernanceActionsToVote } from "@organisms";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@utils";

export const GovernanceActions = () => {
  const { ...dataActionsBarProps } =
    useDataActionsBar();
  const { isMobile, pagePadding } = useScreenDimension();
  const { isEnabled } = useCardano();
  const navigate = useNavigate();
  const { t } = useTranslation();

  useEffect(() => {
    if (isEnabled && getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`)) {
      navigate(PATHS.dashboardGovernanceActions);
    }
  }, [isEnabled]);

  return (
    <Background>
      <ScrollToManage />
      <Box
        sx={{
          display: "flex",
          flexDirection: "column",
          justifyContent: "flex-start",
          minHeight: "100vh",
        }}
      >
        <TopNav />
        <Box
          sx={{
            display: "flex",
            flex: 1,
            flexDirection: "column",
            pt: isMobile ? 3.25 : 6.25,
          }}
        >
          <Typography
            sx={{ mb: isMobile ? 3.75 : 6, px: pagePadding }}
            variant={isMobile ? "title1" : "headline3"}
          >
            {t("govActions.title")}
          </Typography>
          {isMobile && (
            <Divider
              style={{
                borderColor: "#FFFFFF",
                borderWidth: 1,
                marginBottom: 30,
              }}
            />
          )}
          <Box
            sx={{
              display: "flex",
              flex: 1,
              flexDirection: "column",
              px: pagePadding,
            }}
          >
            <DataActionsBar
              {...dataActionsBarProps}
              filterOptions={GOVERNANCE_ACTIONS_FILTERS}
              filtersTitle={t("govActions.filterTitle")}
              sortOptions={GOVERNANCE_ACTIONS_SORTING}
            />
            <Box height={isMobile ? 60 : 80} />
            <GovernanceActionsToVote onDashboard={false} />
          </Box>
        </Box>
        {/* FIXME: Footer should be on top of the layout.
        Should not be rerendered across the pages */}
        <Footer />
      </Box>
    </Background>
  );
};
