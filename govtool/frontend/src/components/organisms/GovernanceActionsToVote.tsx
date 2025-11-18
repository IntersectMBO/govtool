import { Box, CircularProgress } from "@mui/material";

import { Typography } from "@atoms";
import { useCardano, useDataActionsBar } from "@context";
import {
  useFetchNextPageDetector,
  useGetDRepVotesQuery,
  useGetProposalsInfiniteQuery,
  useGetVoterInfo,
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

  const {
    data: votes,
    areDRepVotesLoading,
    isFetching: isFetchingVotes,
  } = useGetDRepVotesQuery(chosenFilters, chosenSorting, debouncedSearchText);

  const saveScrollPosition = useSaveScrollPosition(
    isProposalsLoading,
    isProposalsFetching,
  );

  const mappedData = useMemo(
    () => removeDuplicatedProposals(proposals),
    [proposals, voter?.isRegisteredAsDRep, isProposalsFetchingNextPage],
  );

  // TODO: Filtering here is some kind of craziness. It should be done on the backend.
  const filteredProposals = useMemo(() => {
    const list = mappedData ?? [];
    if (!votes?.length) return list;

    const proposalsFromVotes = votes
      .flatMap((v) => v?.actions ?? [])
      .map((a) => a?.proposal)
      .filter(Boolean);

    const votedKeys = new Set(
      proposalsFromVotes
        .map((p) => ({
          id: p?.id ?? p?.id,
          tx: p?.txHash ?? p?.txHash,
        }))
        .filter(({ id, tx }) => Boolean(id && tx))
        .map(({ id, tx }) => `${id}:${tx}`),
    );

    if (votedKeys.size === 0) return list;

    return list.filter((p) => {
      const id = p?.id ?? p?.id;
      const tx = p?.txHash ?? p?.txHash;
      if (!id || !tx) return true;
      return !votedKeys.has(`${id}:${tx}`);
    });
  }, [mappedData, voter?.isRegisteredAsDRep, isProposalsFetchingNextPage]);

  return (
    <>
      {!filteredProposals ||
      isEnableLoading ||
      isProposalsLoading ||
      areDRepVotesLoading ||
      isFetchingVotes ? (
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
      ) : !filteredProposals?.length ? (
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
          {filteredProposals.map((item) => (
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
