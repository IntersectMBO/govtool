import { useMemo } from "react";
import { Box, Typography, CircularProgress } from "@mui/material";

import { useCardano, useDataActionsBar } from "@context";
import {
  useGetDRepVotesQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { ValidatedGovernanceVotedOnCard } from "@organisms";

type DashboardGovernanceActionsVotedOnProps = {
  searchPhrase?: string;
};

export const DashboardGovernanceActionsVotedOn = ({
  searchPhrase,
}: DashboardGovernanceActionsVotedOnProps) => {
  const { isMobile, screenWidth } = useScreenDimension();
  const { pendingTransaction } = useCardano();
  const { t } = useTranslation();
  const { ...dataActionsBarProps } = useDataActionsBar();
  const { chosenSorting, chosenFilters } = dataActionsBarProps;

  const {
    data: votes,
    areDRepVotesLoading
  } = useGetDRepVotesQuery(chosenFilters, chosenSorting, searchPhrase);

  const proposals = useMemo(() =>
          votes.flatMap((entry) => entry.actions),
      [votes, searchPhrase, pendingTransaction.vote]);

  return areDRepVotesLoading ? (
    <Box py={4} display="flex" justifyContent="center">
      <CircularProgress />
    </Box>
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
        <Box
          columnGap="20px"
          display="grid"
          gridTemplateColumns={`repeat(auto-fill, minmax(${
            screenWidth < 420 ? "290px" : isMobile ? "324px" : "350px"
          }, 1fr))`}
        >
          {proposals.map((item) => (
            <Box pb={4.25} key={item.proposal.txHash + item.proposal.index}>
              <ValidatedGovernanceVotedOnCard
                votedProposal={item}
                inProgress={
                  pendingTransaction.vote?.resourceId ===
                  `${item.proposal.txHash ?? ""}${item.proposal.index ?? ""}`
                }
              />
            </Box>
          ))}
        </Box>
      )}
    </>
  );
};
