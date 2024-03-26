import { useCallback, useMemo, useRef, useState } from "react";
import { generatePath, useNavigate, useParams } from "react-router-dom";
import { Box, CircularProgress, Link } from "@mui/material";

import { Background, Typography } from "@atoms";
import { ICONS, PATHS } from "@consts";
import { useCardano } from "@context";
import { DataActionsBar, GovernanceActionCard } from "@molecules";
import {
  useFetchNextPageDetector,
  useGetProposalsInfiniteQuery,
  useGetVoterInfo,
  useSaveScrollPosition,
  useScreenDimension,
  useTranslation,
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
  const { pendingTransaction, isEnableLoading } = useCardano();
  const { voter } = useGetVoterInfo();
  const { t } = useTranslation();

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
    searchPhrase: searchText,
  });
  const loadNextPageRef = useRef(null);

  useFetchNextPageDetector(
    proposalsfetchNextPage,
    isProposalsLoading || isProposalsFetchingNextPage,
    proposalsHaveNextPage,
  );

  const saveScrollPosition = useSaveScrollPosition(
    isProposalsLoading,
    isProposalsFetching,
  );

  const mappedData = useMemo(() => {
    const uniqueProposals = removeDuplicatedProposals(proposals);

    return uniqueProposals?.filter((i) =>
      getFullGovActionId(i.txHash, i.index)
        .toLowerCase()
        .includes(searchText.toLowerCase()),
    );
  }, [
    proposals,
    voter?.isRegisteredAsDRep,
    searchText,
    isProposalsFetchingNextPage,
  ]);

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
            <Link
              data-testid="back-to-list-link"
              sx={{
                cursor: "pointer",
                display: "flex",
                textDecoration: "none",
                margin: "12px 0 28px",
              }}
              onClick={() => navigate(PATHS.dashboardGovernanceActions)}
            >
              <img
                src={ICONS.arrowRightIcon}
                alt="arrow"
                style={{ marginRight: "12px", transform: "rotate(180deg)" }}
              />
              <Typography variant="body2" color="primary" fontWeight={400}>
                {t("govActions.backToGovActions")}
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
            <Typography
              variant="title2"
              sx={{
                m: "32px 0 32px",
              }}
            >
              {getProposalTypeLabel(category ?? "")}
            </Typography>
            {!mappedData || isEnableLoading || isProposalsLoading ? (
              <Box display="flex" justifyContent="center" py={4}>
                <CircularProgress />
              </Box>
            ) : !mappedData?.length ? (
              <Typography
                sx={{
                  fontWeight: 300,
                  py: 4,
                }}
              >
                <Box display="flex" flexWrap="wrap" mt={4}>
                  <Typography fontWeight={300}>
                    {t("govActions.withCategoryNotExist.partOne")}
                    &nbsp;
                  </Typography>
                  <Typography
                    sx={{
                      fontWeight: 700,
                    }}
                  >
                    {` ${category} `}
                  </Typography>
                  <Typography fontWeight={300}>
                    &nbsp;
                    {t("govActions.withCategoryNotExist.partTwo")}
                  </Typography>
                </Box>
              </Typography>
            ) : (
              <Box
                columnGap="20px"
                display="grid"
                gridTemplateColumns={`repeat(auto-fit, minmax(${
                  screenWidth < 420 ? "290px" : isMobile ? "324px" : "350px"
                }, 1fr))`}
              >
                {mappedData.map((item) => (
                  <Box pb={4.25} key={item.txHash + item.index}>
                    <GovernanceActionCard
                      {...item}
                      index={item.index}
                      inProgress={
                        pendingTransaction.vote?.resourceId ===
                        `${item.txHash ?? ""}${item.index ?? ""}`
                      }
                      // TODO: Add data validation
                      isDataMissing={false}
                      onClick={() => {
                        saveScrollPosition();

                        if (
                          pendingTransaction.vote?.resourceId ===
                          item.txHash + item.index
                        ) {
                          openInNewTab(
                            "https://adanordic.com/latest_transactions",
                          );
                        } else {
                          navigate(
                            generatePath(
                              PATHS.dashboardGovernanceActionsAction,
                              {
                                proposalId: getFullGovActionId(
                                  item.txHash,
                                  item.index,
                                ),
                              },
                            ),
                            {
                              state: {
                                ...item,
                                openedFromCategoryPage: true,
                              },
                            },
                          );
                        }
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
