import { useState, useCallback, useMemo, useRef } from "react";
import {
  NavLink,
  generatePath,
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
  useGetDRepVotesQuery,
  useGetProposalsInfiniteQuery,
  useFetchNextPageDetector,
  useSaveScrollPosition,
  useScreenDimension,
} from "@hooks";
import {
  getFullGovActionId,
  openInNewTab,
  getProposalTypeLabel,
  removeDuplicatedProposals,
} from "@utils";
import { usei18n } from "@translations";

export const DashboardGovernanceActionsCategory = ({}) => {
  const { category } = useParams();
  const [searchText, setSearchText] = useState<string>("");
  const [sortOpen, setSortOpen] = useState(false);
  const [chosenSorting, setChosenSorting] = useState<string>("");
  const { isMobile, screenWidth } = useScreenDimension();
  const navigate = useNavigate();
  const { dRep, voteTransaction } = useCardano();
  const { t } = usei18n();

  const { data: dRepVotes } = useGetDRepVotesQuery([], "");

  const {
    proposals,
    fetchNextPage,
    hasNextPage,
    isFetching,
    isFetchingNextPage,
    isLoading,
  } = useGetProposalsInfiniteQuery(
    [category?.replace(/ /g, "") ?? ""],
    chosenSorting
  );
  const loadNextPageRef = useRef(null);

  useFetchNextPageDetector(
    fetchNextPage,
    isLoading || isFetchingNextPage,
    hasNextPage
  );

  const saveScrollPosition = useSaveScrollPosition(isLoading, isFetching);

  const breadcrumbs = [
    <NavLink
      key="1"
      to={PATHS.dashboard_governance_actions}
      style={{ textDecorationColor: "#0033AD" }}
    >
      <Typography color="primary" fontWeight={300} fontSize={12}>
        {t("govActions.title")}
      </Typography>
    </NavLink>,
    <Typography fontSize={12} fontWeight={500} key="2">
      {getProposalTypeLabel(category ?? "")}
    </Typography>,
  ];

  const mappedData = useMemo(() => {
    const uniqueProposals = removeDuplicatedProposals(proposals);

    if (dRep?.isRegistered && dRepVotes) {
      const filteredBySearchPhrase = uniqueProposals?.filter((i) =>
        getFullGovActionId(i.txHash, i.index)
          .toLowerCase()
          .includes(searchText.toLowerCase())
      );

      const filteredData = filteredBySearchPhrase?.filter((i) => {
        return !dRepVotes
          .flatMap((item) => item.actions.map((item) => item.proposal.id))
          .includes(i.id);
      });

      return filteredData;
    }
    return uniqueProposals?.filter((i) =>
      getFullGovActionId(i.txHash, i.index)
        .toLowerCase()
        .includes(searchText.toLowerCase())
    );
  }, [
    proposals,
    dRep?.isRegistered,
    dRepVotes,
    searchText,
    isFetchingNextPage,
  ]);

  const closeSorts = useCallback(() => {
    setSortOpen(false);
  }, [setSortOpen]);

  return (
    <Background>
      <Box
        minHeight={"100vh"}
        display="flex"
        flexDirection={"column"}
        justifyContent="flex-start"
      >
        <Box flex={1}>
          <Box px={isMobile ? 2 : 3.75} flex={1}>
            <Breadcrumbs
              separator="|"
              aria-label="breadcrumb"
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
              <Typography variant="body2" color="primary">
                {t("backToList")}
              </Typography>
            </Link>
            <DataActionsBar
              isFiltering={false}
              searchText={searchText}
              setSearchText={setSearchText}
              sortOpen={sortOpen}
              setSortOpen={setSortOpen}
              sortingActive={Boolean(chosenSorting)}
              chosenSorting={chosenSorting}
              setChosenSorting={setChosenSorting}
              closeSorts={closeSorts}
            />
            <Box height={24} />
            {!isLoading ? (
              !mappedData?.length ? (
                <Typography py={4} fontWeight="300">
                  <Box mt={4} display="flex" flexWrap="wrap">
                    <Typography fontWeight={300}>
                      {t("govActions.withCategoryNotExist.partOne")}&nbsp;
                    </Typography>
                    <Typography fontWeight="bold">{` ${category} `}</Typography>
                    <Typography fontWeight={300}>
                      &nbsp;{t("govActions.withCategoryNotExist.partTwo")}
                    </Typography>
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
                        txHash={item.txHash}
                        index={item.index}
                        inProgress={
                          voteTransaction.proposalId ===
                          item.txHash + item.index
                        }
                        onClick={() => {
                          saveScrollPosition();

                          voteTransaction.proposalId ===
                          item.txHash + item.index
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
                      />
                    </Box>
                  ))}
                  {hasNextPage && isFetchingNextPage && (
                    <Box
                      py={4}
                      display="flex"
                      justifyContent="center"
                      ref={loadNextPageRef}
                    >
                      <CircularProgress />
                    </Box>
                  )}
                </Box>
              )
            ) : (
              <Box py={4} display="flex" justifyContent="center">
                <CircularProgress />
              </Box>
            )}
          </Box>
        </Box>
      </Box>
    </Background>
  );
};
