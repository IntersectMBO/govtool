import { useState, useEffect, useCallback, useMemo } from "react";
import { Box, CircularProgress, Tab, Tabs, styled } from "@mui/material";
import { useLocation, useNavigate } from "react-router-dom";

import {
  GOVERNANCE_ACTIONS_FILTERS,
  GOVERNANCE_ACTIONS_SORTING,
  PATHS,
  PDF_PATHS,
} from "@consts";
import { useCardano, useDataActionsBar, useFeatureFlag } from "@context";
import {
  useGetDRepVotesQuery,
  useGetProposalsQuery,
  useGetVoterInfo,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { DataActionsBar } from "@molecules";
import {
  GovernanceActionsToVote,
  DashboardGovernanceActionsVotedOn,
} from "@organisms";
import { Button } from "@atoms";
import usePrevious from "@/hooks/usePrevious";

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
  const { debouncedSearchText, isAdjusting, ...dataActionsBarProps } =
    useDataActionsBar();
  const { chosenFilters, chosenSorting } = dataActionsBarProps;
  const { voter } = useGetVoterInfo();
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();
  const { isEnableLoading } = useCardano();
  const { isProposalDiscussionForumEnabled } = useFeatureFlag();
  const navigate = useNavigate();

  const queryFilters =
    chosenFilters.length > 0 ? chosenFilters : defaultCategories;

  const prevFilters = usePrevious(queryFilters);
  const prevSorting = usePrevious(chosenSorting);

  const stableFilters = isAdjusting
    ? prevFilters ?? queryFilters
    : queryFilters;
  const stableSorting = isAdjusting
    ? prevSorting ?? chosenSorting
    : chosenSorting;

  const { proposals, isProposalsLoading } = useGetProposalsQuery({
    filters: stableFilters,
    sorting: stableSorting,
    searchPhrase: debouncedSearchText,
    enabled: true,
  });
  const { data: votes, areDRepVotesLoading } = useGetDRepVotesQuery(
    queryFilters,
    chosenSorting,
    debouncedSearchText,
  );

  // White Magic :)
  const shouldFilter =
  voter?.isRegisteredAsDRep || voter?.isRegisteredAsSoleVoter;

const filteredProposals = useMemo(() => {
  if (!shouldFilter || !proposals || !votes) return proposals;

  return proposals
    .map((proposalCategory) => {
      const filteredActions = proposalCategory.actions.filter((action) => {
        const hasVote = votes.some((voteCategory) =>
          voteCategory.actions.some(
            (voteAction) =>
              voteAction.proposal.txHash === action.txHash &&
              voteAction.proposal.index === action.index,
          ),
        );
        return !hasVote;
      });

      return {
        ...proposalCategory,
        actions: filteredActions,
      };
    })
    .filter((category) => category.actions.length > 0);
}, [proposals, votes, shouldFilter]);

  const { state } = useLocation();
  const [content, setContent] = useState<number>(
    state?.isVotedListOnLoad ? 1 : 0,
  );

  const handleChange = (_event: React.SyntheticEvent, newValue: number) => {
    setContent(newValue);
  };

  const onClickPropose = useCallback(() => {
    navigate(
      isProposalDiscussionForumEnabled
        ? PDF_PATHS.proposalDiscussionPropose
        : PATHS.createGovernanceAction,
    );
  }, [isProposalDiscussionForumEnabled]);

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
              <Box display="flex" flexDirection="row" alignItems="center">
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
                  aria-label="Governance Actions tabs"
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
                <Button
                  data-testid="proposal-discussion-link"
                  onClick={onClickPropose}
                  sx={{
                    display: isMobile ? "none" : "block",
                    ml: "auto",
                  }}
                >
                  {t("govActions.propose")}
                </Button>
              </Box>
            )}

            <Box height={isMobile ? 24 : 60} />
            <CustomTabPanel value={content} index={0}>
              <GovernanceActionsToVote
                filters={chosenFilters}
                onDashboard
                searchPhrase={debouncedSearchText}
                sorting={chosenSorting}
                proposals={filteredProposals}
              />
            </CustomTabPanel>
            <CustomTabPanel value={content} index={1}>
              <DashboardGovernanceActionsVotedOn
                searchPhrase={debouncedSearchText}
                votes={votes}
                areDRepVotesLoading={areDRepVotesLoading}
              />
            </CustomTabPanel>
          </>
        )}
      </>
    </Box>
  );
};
