import { useEffect } from "react";
import { useNavigate } from "react-router-dom";
import { Box, CircularProgress, Divider } from "@mui/material";

import { Background, ScrollToManage, Typography } from "@atoms";
import {
  GOVERNANCE_ACTIONS_FILTERS,
  GOVERNANCE_ACTIONS_SORTING,
  PATHS,
} from "@consts";
import { useCardano, useDataActionsBar } from "@context";
import {
  useGetProposalsQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { DataActionsBar } from "@molecules";
import { Footer, TopNav, GovernanceActionsToVote } from "@organisms";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@utils";

const defaultCategories = GOVERNANCE_ACTIONS_FILTERS.map(
  (category) => category.key,
);

export const GovernanceActions = () => {
  const { debouncedSearchText, isAdjusting, ...dataActionsBarProps } =
    useDataActionsBar();
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
    enabled: !isAdjusting,
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
            {!proposals || isProposalsLoading ? (
              <Box
                sx={{
                  alignItems: "center",
                  display: "flex",
                  flex: 1,
                  justifyContent: "center",
                }}
              >
                <CircularProgress />
              </Box>
            ) : (
              <>
                <Box height={isMobile ? 60 : 80} />
                <GovernanceActionsToVote
                  filters={chosenFilters}
                  onDashboard={false}
                  searchPhrase={debouncedSearchText}
                  sorting={chosenSorting}
                  proposals={proposals}
                />
              </>
            )}
          </Box>
        </Box>
        {/* FIXME: Footer should be on top of the layout.
        Should not be rerendered across the pages */}
        <Footer />
      </Box>
    </Background>
  );
};
