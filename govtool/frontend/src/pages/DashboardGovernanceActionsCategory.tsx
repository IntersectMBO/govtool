import { useMemo, useRef } from "react";
import { generatePath, useNavigate, useParams } from "react-router-dom";
import { Box, CircularProgress, Link } from "@mui/material";

import { Background, Typography } from "@atoms";
import { GOVERNANCE_ACTIONS_SORTING, ICONS, PATHS } from "@consts";
import { useCardano, useDataActionsBar } from "@context";
import {
  DataActionsBar,
  EmptyStateGovernanceActionsCategory,
  GovernanceActionCard,
} from "@molecules";
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
  const { debouncedSearchText, ...dataActionsBarProps } = useDataActionsBar();
  const { chosenSorting } = dataActionsBarProps;
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

  const mappedData = useMemo(
    () => removeDuplicatedProposals(proposals),
    [proposals, voter?.isRegisteredAsDRep, isProposalsFetchingNextPage],
  );

  return (
    <Background>
      <Box
        sx={{
          display: "flex",
          flexDirection: "column",
          flex: 1,
          px: isMobile ? 2 : 3.75,
        }}
      >
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
          {...dataActionsBarProps}
          isFiltering={false}
          sortOptions={GOVERNANCE_ACTIONS_SORTING}
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
          <Box
            sx={{
              alignItems: "center",
              display: "flex",
              flex: 1,
              justifyContent: "center",
              py: 4,
            }}
          >
            <CircularProgress />
          </Box>
        ) : !mappedData?.length ? (
          <EmptyStateGovernanceActionsCategory
            category={category}
            isSearch={!!debouncedSearchText.length}
          />
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
                  inProgress={
                    pendingTransaction.vote?.resourceId ===
                    `${item.txHash ?? ""}${item.index ?? ""}`
                  }
                  onClick={() => {
                    saveScrollPosition();

                    if (
                      pendingTransaction.vote?.resourceId ===
                      item.txHash + item.index
                    ) {
                      openInNewTab(
                        `https://sancho.cexplorer.io/tx/${pendingTransaction.vote.transactionHash}`,
                      );
                    } else {
                      navigate(
                        generatePath(PATHS.dashboardGovernanceActionsAction, {
                          proposalId: getFullGovActionId(
                            item.txHash,
                            item.index,
                          ),
                        }),
                        {
                          state: {
                            ...item,
                            openedFromCategoryPage: true,
                            isDataMissing: item.metadataStatus.raw.valid
                              ? false
                              : item.metadataStatus.raw.status,
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
    </Background>
  );
};
