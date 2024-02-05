import { useCallback, useMemo, useRef, useState } from "react";
import {
  generatePath,
  NavLink,
  useNavigate,
  useParams,
} from "react-router-dom";
import {
  Box,
  Breadcrumbs,
  CircularProgress,
  Link,
  Typography,
} from "@mui/material";

import { Background } from "@atoms";
import { ICONS, PATHS } from "@consts";
import { useCardano } from "@context";
import { DataActionsBar, GovernanceActionCard } from "@molecules";
import {
  useFetchNextPageDetector,
  useGetProposalsInfiniteQuery,
  useSaveScrollPosition,
  useScreenDimension,
} from "@hooks";
import {
  getFullGovActionId,
  getProposalTypeLabel,
  openInNewTab,
  removeDuplicatedProposals,
} from "@utils";

export const DashboardGovernanceActionsCategory = () => {
  const { category } = useParams();
  const [searchText, setSearchText] = useState<string>("");
  const [sortOpen, setSortOpen] = useState(false);
  const [chosenSorting, setChosenSorting] = useState<string>("");
  const { isMobile, screenWidth } = useScreenDimension();
  const navigate = useNavigate();
  const { dRep, isDrepLoading, voteTransaction } = useCardano();

  const {
    isProposalsFetching,
    isProposalsFetchingNextPage,
    isProposalsLoading,
    proposals,
    proposalsfetchNextPage,
    proposalsHaveNextPage,
  } = useGetProposalsInfiniteQuery({
    filters: [category?.replace(/ /g, "") ?? ""],
    sorting: chosenSorting,
  });
  const loadNextPageRef = useRef(null);

  useFetchNextPageDetector(
    proposalsfetchNextPage,
    isProposalsLoading || isProposalsFetchingNextPage,
    proposalsHaveNextPage
  );

  const saveScrollPosition = useSaveScrollPosition(
    isProposalsLoading,
    isProposalsFetching
  );

  const breadcrumbs = [
    <NavLink
      key="1"
      style={{ textDecorationColor: "#0033AD" }}
      to={PATHS.dashboard_governance_actions}
    >
      <Typography color="primary" fontSize={12} fontWeight={300}>
        Governance Actions
      </Typography>
    </NavLink>,
    <Typography fontSize={12} fontWeight={500} key="2">
      {getProposalTypeLabel(category ?? "")}
    </Typography>,
  ];

  const mappedData = useMemo(() => {
    const uniqueProposals = removeDuplicatedProposals(proposals);

    return uniqueProposals?.filter((i) =>
      getFullGovActionId(i.txHash, i.index)
        .toLowerCase()
        .includes(searchText.toLowerCase())
    );
  }, [proposals, dRep?.isRegistered, searchText, isProposalsFetchingNextPage]);

  const closeSorts = useCallback(() => {
    setSortOpen(false);
  }, [setSortOpen]);

  return (
    <Background>
      <Box
        display="flex"
        flexDirection="column"
        justifyContent="flex-start"
        minHeight="100vh"
      >
        <Box flex={1}>
          <Box px={isMobile ? 2 : 3.75} flex={1}>
            <Breadcrumbs
              aria-label="breadcrumb"
              separator="|"
              sx={{
                marginBottom: isMobile ? 3 : 5,
                marginTop: isMobile ? 2.5 : 1.25,
              }}
            >
              {breadcrumbs}
            </Breadcrumbs>
            <Link
              data-testid={"back-to-list-link"}
              sx={{
                cursor: "pointer",
                display: "flex",
                textDecoration: "none",
                marginBottom: 3,
              }}
              onClick={() => navigate(PATHS.dashboard_governance_actions)}
            >
              <img
                src={ICONS.arrowRightIcon}
                alt="arrow"
                style={{ marginRight: "12px", transform: "rotate(180deg)" }}
              />
              <Typography color="primary" variant="body2">
                Back to the list
              </Typography>
            </Link>
            <DataActionsBar
              chosenSorting={chosenSorting}
              closeSorts={closeSorts}
              isFiltering={false}
              searchText={searchText}
              setChosenSorting={setChosenSorting}
              setSearchText={setSearchText}
              setSortOpen={setSortOpen}
              sortingActive={Boolean(chosenSorting)}
              sortOpen={sortOpen}
            />
            <Box height={24} />
            {isProposalsLoading || isDrepLoading ? (
              <Box display="flex" justifyContent="center" py={4}>
                <CircularProgress />
              </Box>
            ) : !mappedData?.length ? (
              <Typography fontWeight="300" py={4}>
                <Box display="flex" flexWrap="wrap" mt={4}>
                  <Typography fontWeight={300}>
                    Governnance actions with category&nbsp;
                  </Typography>
                  <Typography fontWeight="bold">{` ${category} `}</Typography>
                  <Typography fontWeight={300}>&nbsp;don't exist.</Typography>
                </Box>
              </Typography>
            ) : (
              <Box
                columnGap={4}
                display="grid"
                gridTemplateColumns={`repeat(auto-fit, minmax(${
                  screenWidth < 375
                    ? "255px"
                    : screenWidth < 768
                    ? "294px"
                    : "402px"
                }, 1fr))`}
              >
                {mappedData.map((item) => (
                  <Box pb={4.25} key={item.txHash + item.index}>
                    <GovernanceActionCard
                      {...item}
                      index={item.index}
                      inProgress={
                        voteTransaction.proposalId === item.txHash + item.index
                      }
                      onClick={() => {
                        saveScrollPosition();

                        voteTransaction.proposalId === item.txHash + item.index
                          ? openInNewTab(
                              "https://adanordic.com/latest_transactions"
                            )
                          : navigate(
                              generatePath(
                                PATHS.dashboard_governance_actions_action,
                                {
                                  proposalId: getFullGovActionId(
                                    item.txHash,
                                    item.index
                                  ),
                                }
                              ),
                              {
                                state: {
                                  ...item,
                                  openedFromCategoryPage: true,
                                },
                              }
                            );
                      }}
                      txHash={item.txHash}
                    />
                  </Box>
                ))}
                {proposalsHaveNextPage && isProposalsFetchingNextPage && (
                  <Box
                    display="flex"
                    justifyContent="center"
                    py={4}
                    ref={loadNextPageRef}
                  >
                    <CircularProgress />
                  </Box>
                )}
              </Box>
            )}
          </Box>
        </Box>
      </Box>
    </Background>
  );
};
