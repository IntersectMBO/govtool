import { useEffect, useMemo, useRef } from "react";
import { useNavigate, useParams } from "react-router-dom";
import { Box, CircularProgress, Link } from "@mui/material";

import { Background, Typography } from "@atoms";
import { ICONS, PATHS } from "@consts";
import { useCardano } from "@context";
import { DataActionsBar, GovernanceActionCard } from "@molecules";
import { Footer, TopNav } from "@organisms";
import {
  useGetProposalsInfiniteQuery,
  useFetchNextPageDetector,
  useSaveScrollPosition,
  useScreenDimension,
  useTranslation,
  useGetVoterInfo,
  useDataActionsBar,
} from "@hooks";
import {
  WALLET_LS_KEY,
  getFullGovActionId,
  getItemFromLocalStorage,
  getProposalTypeLabel,
  removeDuplicatedProposals,
} from "@utils";

export const GovernanceActionsCategory = () => {
  const { category } = useParams();
  const { debouncedSearchText, ...dataActionsBarProps } = useDataActionsBar();
  const { chosenSorting } = dataActionsBarProps;
  const { isMobile, pagePadding, screenWidth } = useScreenDimension();
  const { isEnabled } = useCardano();
  const navigate = useNavigate();
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
    searchPhrase: debouncedSearchText,
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

  const mappedData = useMemo(() => removeDuplicatedProposals(proposals), [
    voter?.isRegisteredAsDRep,
    isProposalsFetchingNextPage,
    proposals,
  ]);

  useEffect(() => {
    if (isEnabled && getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`)) {
      const { pathname } = window.location;
      navigate(`/connected${pathname}`);
    }
  }, [isEnabled]);

  return (
    <Background>
      <Box
        display="flex"
        flexDirection="column"
        justifyContent="flex-start"
        minHeight="100vh"
      >
        <TopNav />
        <Box flex={1} mt={isMobile ? 3.25 : 6.25} overflow="hidden">
          <Box px={pagePadding} flex={1}>
            <Link
              data-testid="back-to-list-link"
              sx={{
                cursor: "pointer",
                display: "flex",
                textDecoration: "none",
                marginBottom: 4.25,
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
              {...dataActionsBarProps}
              isFiltering={false}
            />
            <Typography
              variant="title2"
              sx={{
                m: "32px 0 32px",
              }}
            >
              {getProposalTypeLabel(category ?? "")}
            </Typography>
            {!isProposalsLoading ? (
              !mappedData?.length ? (
                <Typography fontWeight={300} sx={{ py: 4 }}>
                  <Box mt={4} display="flex" flexWrap="wrap">
                    <Typography fontWeight={300}>
                      {t("govActions.withCategoryNotExist.partOne")}
                      &nbsp;
                    </Typography>
                    <Typography fontWeight={700}>
                      {category}
                      &nbsp;
                    </Typography>
                    {debouncedSearchText && (
                      <>
                        <Typography fontWeight={300}>
                          {t("govActions.withCategoryNotExist.optional")}
                          &nbsp;
                        </Typography>
                        <Typography fontWeight={700}>{debouncedSearchText}</Typography>
                      </>
                    )}
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
                        txHash={item.txHash}
                        index={item.index}
                        // TODO: Add data validation
                        isDataMissing={false}
                        onClick={() => {
                          saveScrollPosition();

                          navigate(
                            PATHS.governanceActionsAction.replace(
                              ":proposalId",
                              getFullGovActionId(item.txHash, item.index),
                            ),
                            {
                              state: {
                                ...item,
                                openedFromCategoryPage: true,
                              },
                            },
                          );
                        }}
                      />
                    </Box>
                  ))}
                  {proposalsHaveNextPage && isProposalsFetchingNextPage && (
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
        <Footer />
      </Box>
    </Background>
  );
};
