import { useState, useCallback, useEffect } from "react";
import { useNavigate } from "react-router-dom";
import { Box, Divider } from "@mui/material";

import { Background, ScrollToManage, Typography } from "@atoms";
import { PATHS } from "@consts";
import { useCardano } from "@context";
import { useScreenDimension } from "@hooks";
import { DataActionsBar } from "@molecules";
import { Footer, TopNav, GovernanceActionsToVote } from "@organisms";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@utils";
import { usei18n } from "@translations";

export const GovernanceActions = () => {
  const [searchText, setSearchText] = useState<string>("");
  const [filtersOpen, setFiltersOpen] = useState(false);
  const [chosenFilters, setChosenFilters] = useState<string[]>([]);
  const [sortOpen, setSortOpen] = useState(false);
  const [chosenSorting, setChosenSorting] = useState<string>("");
  const { isMobile, pagePadding } = useScreenDimension();
  const { isEnabled } = useCardano();
  const navigate = useNavigate();
  const { t } = usei18n();

  useEffect(() => {
    if (isEnabled && getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`)) {
      navigate(PATHS.dashboard_governance_actions);
    }
  }, [isEnabled]);

  const closeFilters = useCallback(() => {
    setFiltersOpen(false);
  }, [setFiltersOpen]);

  const closeSorts = useCallback(() => {
    setSortOpen(false);
  }, [setSortOpen]);

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
            <DataActionsBar
              chosenFilters={chosenFilters}
              chosenFiltersLength={chosenFilters.length}
              chosenSorting={chosenSorting}
              closeFilters={closeFilters}
              closeSorts={closeSorts}
              filtersOpen={filtersOpen}
              searchText={searchText}
              setChosenFilters={setChosenFilters}
              setChosenSorting={setChosenSorting}
              setFiltersOpen={setFiltersOpen}
              setSearchText={setSearchText}
              setSortOpen={setSortOpen}
              sortingActive={Boolean(chosenSorting)}
              sortOpen={sortOpen}
            />
            <Box height={isMobile ? 60 : 80} />
            <GovernanceActionsToVote
              filters={chosenFilters}
              onDashboard={false}
              searchPhrase={searchText}
              sorting={chosenSorting}
            />
          </Box>
        </Box>
        <Footer />
      </Box>
    </Background>
  );
};
