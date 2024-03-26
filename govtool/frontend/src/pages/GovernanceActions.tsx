import { useEffect } from "react";
import { useNavigate } from "react-router-dom";
import { Box, CircularProgress, Divider } from "@mui/material";

import { Background, ScrollToManage, Typography } from "@atoms";
import { GOVERNANCE_ACTIONS_FILTERS, PATHS } from "@consts";
import { useCardano } from "@context";
import {
  useDataActionsBar,
  useGetProposalsQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { DataActionsBar } from "@molecules";
import { Footer, TopNav, GovernanceActionsToVote } from "@organisms";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@utils";

const defaultCategories = GOVERNANCE_ACTIONS_FILTERS.map(
  (category) => category.key
);

export const GovernanceActions = () => {
  const { debouncedSearchText, ...dataActionsBarProps } = useDataActionsBar();
  const { chosenFilters, chosenSorting } = dataActionsBarProps;
  const { isMobile, pagePadding } = useScreenDimension();
  const { isEnabled } = useCardano();
  const navigate = useNavigate();
  const { t } = useTranslation();

  const queryFilters =
    chosenFilters.length > 0 ? chosenFilters : defaultCategories;

  const { proposals, isProposalsLoading } = useGetProposalsQuery({
    filters: queryFilters,
    sorting: chosenSorting,
    searchPhrase: debouncedSearchText,
  });

  useEffect(() => {
    if (isEnabled && getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`)) {
      navigate(PATHS.dashboardGovernanceActions);
    }
  }, [isEnabled]);

  return (
    <Background>
      <ScrollToManage />
      <Box
        display="flex"
        flexDirection="column"
        justifyContent="flex-start"
        minHeight="100vh"
      >
        <TopNav />
        <Box flex={1} mt={isMobile ? 3.25 : 6.25} overflow="hidden">
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
          <Box flex={1} px={pagePadding}>
            <DataActionsBar {...dataActionsBarProps} />
            <Box height={isMobile ? 60 : 80} />
            {!proposals || isProposalsLoading ? (
              <Box
                alignItems="center"
                display="flex"
                flex={1}
                height="100%"
                justifyContent="center"
              >
                <CircularProgress />
              </Box>
            ) : (
              <GovernanceActionsToVote
                filters={chosenFilters}
                onDashboard={false}
                searchPhrase={debouncedSearchText}
                sorting={chosenSorting}
                proposals={proposals}
              />
            )}
          </Box>
        </Box>
        <Footer />
      </Box>
    </Background>
  );
};
