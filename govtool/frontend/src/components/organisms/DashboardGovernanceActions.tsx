import { useState, useCallback, useEffect } from "react";
import { Box, CircularProgress, Tab, Tabs, styled } from "@mui/material";
import { useLocation } from "react-router-dom";

import { useCardano } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";
import { DataActionsBar } from "@molecules";
import {
  GovernanceActionsToVote,
  DashboardGovernanceActionsVotedOn,
} from "@organisms";

interface TabPanelProps {
  children?: React.ReactNode;
  index: number;
  value: number;
}

function CustomTabPanel(props: TabPanelProps) {
  const { children, value, index } = props;

  return (
    <div
      role="tabpanel"
      hidden={value !== index}
      id={`simple-tabpanel-${index}`}
      aria-labelledby={`simple-tab-${index}`}
      style={{
        display: "flex",
        flexDirection: "column",
        flex: value !== index ? 0 : 1,
      }}
    >
      {value === index && <Box pb={3}>{children}</Box>}
    </div>
  );
}

type StyledTabProps = {
  label: string;
};

const StyledTab = styled((props: StyledTabProps) => (
  <Tab disableRipple {...props} />
))(() => ({
  textTransform: "none",
  fontWeight: 400,
  fontSize: 16,
  color: "#242232",
  "&.Mui-selected": {
    color: "#FF640A",
    fontWeight: 500,
  },
}));

export const DashboardGovernanceActions = () => {
  const [searchText, setSearchText] = useState<string>("");
  const [filtersOpen, setFiltersOpen] = useState(false);
  const [chosenFilters, setChosenFilters] = useState<string[]>([]);
  const [sortOpen, setSortOpen] = useState(false);
  const [chosenSorting, setChosenSorting] = useState<string>("");

  const { state } = useLocation();
  const [content, setContent] = useState<number>(
    state && state.isVotedListOnLoad ? 1 : 0
  );

  const { dRep, isDrepLoading } = useCardano();
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  const handleChange = (_event: React.SyntheticEvent, newValue: number) => {
    setContent(newValue);
  };

  const closeFilters = useCallback(() => {
    setFiltersOpen(false);
  }, [setFiltersOpen]);

  const closeSorts = useCallback(() => {
    setSortOpen(false);
  }, [setSortOpen]);

  useEffect(() => {
    window.history.replaceState({}, document.title);
  }, []);

  return (
    <Box
      px={isMobile ? 2 : 3.5}
      pt={6}
      flex={1}
      display="flex"
      flexDirection="column"
    >
      {isDrepLoading ? (
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
        <>
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
          {dRep?.isRegistered && (
            <Tabs
              sx={{
                marginTop: 3,
                display: "flex",
                fontSize: 16,
                fontWeight: 500,
              }}
              value={content}
              indicatorColor="secondary"
              onChange={handleChange}
              aria-label="basic tabs example"
            >
              <StyledTab
                data-testid="to-vote-tab"
                label={t("govActions.toVote")}
                sx={{
                  textTransform: "none",
                  width: !isMobile ? "auto" : "50%",
                }}
              />
              <StyledTab
                data-testid="voted-tab"
                label={t("govActions.voted")}
                sx={{
                  textTransform: "none",
                  width: !isMobile ? "auto" : "50%",
                }}
              />
            </Tabs>
          )}
          <Box height={isMobile ? 24 : 60} />
          <CustomTabPanel value={content} index={0}>
            <GovernanceActionsToVote
              filters={chosenFilters}
              onDashboard
              searchPhrase={searchText}
              sorting={chosenSorting}
            />
          </CustomTabPanel>
          <CustomTabPanel value={content} index={1}>
            <DashboardGovernanceActionsVotedOn
              filters={chosenFilters}
              searchPhrase={searchText}
              sorting={chosenSorting}
            />
          </CustomTabPanel>
        </>
      )}
    </Box>
  );
};
