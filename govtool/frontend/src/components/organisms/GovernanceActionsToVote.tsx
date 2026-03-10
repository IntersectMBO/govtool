import { Box, CircularProgress } from "@mui/material";

import { Typography } from "@atoms";
import { useCardano, useDataActionsBar } from "@context";
import {
  useFetchNextPageDetector,
  useGetProposalsInfiniteQuery,
  useSaveScrollPosition,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { removeDuplicatedProposals } from "@utils";
import { ValidatedGovernanceActionCard } from "@organisms";
import { useMemo, useRef } from "react";

type GovernanceActionsToVoteProps = {
  onDashboard?: boolean;
};

export const GovernanceActionsToVote = ({
  onDashboard = true,
}: GovernanceActionsToVoteProps) => {
  const { pendingTransaction, isEnableLoading } = useCardano();
  const { isMobile, screenWidth } = useScreenDimension();
  const { debouncedSearchText, ...dataActionsBarProps } = useDataActionsBar();
  const { chosenSorting, chosenFilters } = dataActionsBarProps;
  const { t } = useTranslation();

  const {
    isProposalsFetching,
    isProposalsFetchingNextPage,
    isProposalsLoading,
    proposals,
    proposalsfetchNextPage,
    proposalsHaveNextPage,
  } = useGetProposalsInfiniteQuery({
    filters: chosenFilters,
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

  const mappedProposals = useMemo(
    () => removeDuplicatedProposals(proposals),
    [proposals, isProposalsFetchingNextPage],
  );

  return (
    <>
      {!mappedProposals ||
      isEnableLoading ||
      isProposalsLoading ? (
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
      ) : !mappedProposals?.length ? (
        <Typography fontWeight={300} sx={{ py: 4 }}>
          {t("govActions.noResultsForTheSearch")}
        </Typography>
      ) : (
        <Box
          columnGap="20px"
          display="grid"
          gridTemplateColumns={`repeat(auto-fill, minmax(${
            screenWidth < 420 ? "290px" : isMobile ? "324px" : "350px"
          }, 1fr))`}
        >
          {mappedProposals.map((item) => (
            <Box pb={4.25} key={item.txHash + item.index}>
              <ValidatedGovernanceActionCard
                {...item}
                inProgress={
                  onDashboard &&
                  pendingTransaction.vote?.resourceId ===
                    `${item.txHash ?? ""}${item.index ?? ""}`
                }
                onClick={() => {
                  saveScrollPosition();
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
    </>
  );
};
