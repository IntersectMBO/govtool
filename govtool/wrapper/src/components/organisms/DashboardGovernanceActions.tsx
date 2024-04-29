import { useState, useEffect } from "react";
import { Box, CircularProgress, Tab, Tabs, styled } from "@mui/material";
import { useLocation } from "react-router-dom";

import {
  GovernanceActionsToVote,
  DashboardGovernanceActionsVotedOn,
} from "@govtool/voting";

import {
  GOVERNANCE_ACTIONS_FILTERS,
  GOVERNANCE_ACTIONS_SORTING,
} from "@consts";
import { useCardano, useDataActionsBar } from "@context";
import {
  useGetProposalsQuery,
  useGetVoterInfo,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { DataActionsBar } from "@molecules";

type TabPanelProps = {
  children?: React.ReactNode;
  index: number;
  value: number;
};

const defaultCategories = GOVERNANCE_ACTIONS_FILTERS.map(
  (category) => category.key,
);

const CustomTabPanel = (props: TabPanelProps) => {
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
};

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
  const { debouncedSearchText, ...dataActionsBarProps } = useDataActionsBar();
  const { chosenFilters, chosenSorting } = dataActionsBarProps;
  const { voter } = useGetVoterInfo();
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();
  const { isEnableLoading } = useCardano();

  const queryFilters =
    chosenFilters.length > 0 ? chosenFilters : defaultCategories;

  const { proposals, isProposalsLoading } = useGetProposalsQuery({
    filters: queryFilters,
    sorting: chosenSorting,
    searchPhrase: debouncedSearchText,
  });

  const { state } = useLocation();
  const [content, setContent] = useState<number>(
    state && state.isVotedListOnLoad ? 1 : 0,
  );

  const handleChange = (_event: React.SyntheticEvent, newValue: number) => {
    setContent(newValue);
  };

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
      <>
        <DataActionsBar
          {...dataActionsBarProps}
          filterOptions={GOVERNANCE_ACTIONS_FILTERS}
          filtersTitle={t("govActions.filterTitle")}
          sortOptions={GOVERNANCE_ACTIONS_SORTING}
        />
        {!proposals || !voter || isEnableLoading || isProposalsLoading ? (
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
            {(voter?.isRegisteredAsDRep || voter?.isRegisteredAsSoleVoter) && (
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
                  label={t("govActions.votedOnByMe")}
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
                searchPhrase={debouncedSearchText}
                sorting={chosenSorting}
                proposals={proposals}
              />
            </CustomTabPanel>
            <CustomTabPanel value={content} index={1}>
              <DashboardGovernanceActionsVotedOn
                filters={chosenFilters}
                searchPhrase={debouncedSearchText}
                sorting={chosenSorting}
              />
            </CustomTabPanel>
          </>
        )}
      </>
    </Box>
  );
};
