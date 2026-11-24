import { useMemo } from "react";
import { Box, Typography, CircularProgress } from "@mui/material";

import { useCardano, useDataActionsBar } from "@context";
import {
  useGetDRepVotesQuery,
  useTranslation,
} from "@hooks";
import { SearchNotReady } from "@molecules";
import { ValidatedGovernanceVotedOnCard } from "@organisms";

type DashboardGovernanceActionsVotedOnProps = {
  searchPhrase?: string;
};

export const DashboardGovernanceActionsVotedOn = ({
  searchPhrase,
}: DashboardGovernanceActionsVotedOnProps) => {
  const { pendingTransaction } = useCardano();
  const { t } = useTranslation();
  const { ...dataActionsBarProps } = useDataActionsBar();
  const { chosenSorting, chosenFilters } = dataActionsBarProps;

  const {
    data: votes,
    areDRepVotesLoading,
    isSearchNotReady,
  } = useGetDRepVotesQuery(chosenFilters, chosenSorting, searchPhrase);

  const proposals = useMemo(() =>
          votes.flatMap((entry) => entry.actions),
      [votes, searchPhrase, pendingTransaction.vote]);

  return areDRepVotesLoading ? (
    <Box py={4} display="flex" justifyContent="center">
      <CircularProgress />
    </Box>
  ) : isSearchNotReady ? (
    <SearchNotReady />
  ) : (
    <>
      {!votes.length ? (
        <Typography py={4} fontWeight="300">
          {t("govActions.youHaventVotedYet")}
        </Typography>
      ) : !proposals?.length ? (
        <Typography py={4} fontWeight="300">
          {t("govActions.noResultsForTheSearch")}
        </Typography>
      ) : (
        <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
          {proposals.map((item) => (
            <ValidatedGovernanceVotedOnCard
              key={item.proposal.txHash + item.proposal.index}
              votedProposal={item}
              inProgress={
                pendingTransaction.vote?.resourceId ===
                `${item.proposal.txHash ?? ""}${item.proposal.index ?? ""}`
              }
            />
          ))}
        </Box>
      )}
    </>
  );
};
