import { useMemo } from "react";
import { Box, Typography, CircularProgress } from "@mui/material";

import { useCardano, useDataActionsBar } from "@context";
import {
  useGetDRepVotesQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { ValidatedGovernanceVotedOnCard } from "@organisms";
import { getFullGovActionId } from "@utils";

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
    areDRepVotesLoading,
    isFetching,
  } = useGetDRepVotesQuery(chosenFilters, chosenSorting, searchPhrase);

  // TODO: Filtering here is some kind of craziness. It should be done on the backend.
  const filteredData = useMemo(() => {
    if (!votes?.length) return [];
    if (!searchPhrase) return votes.flatMap((entry) => entry.actions);

    const lowerSearch = searchPhrase.toLowerCase();

    return votes.flatMap((entry) =>
      entry.actions.filter((action) => {
        const hash = getFullGovActionId(
          action.proposal.txHash,
          action.proposal.index,
        ).toLowerCase();

        const title = action.proposal.title?.toLowerCase() || "";
        const motivation = action.proposal.motivation?.toLowerCase() || "";
        const rationale = action.proposal.rationale?.toLowerCase() || "";
        const abstract = action.proposal.abstract?.toLowerCase() || "";

        return (
          hash.includes(lowerSearch) ||
          title.includes(lowerSearch) ||
          motivation.includes(lowerSearch) ||
          rationale.includes(lowerSearch) ||
          abstract.includes(lowerSearch)
        );
      }),
    );
  }, [votes, searchPhrase, pendingTransaction.vote]);

  return areDRepVotesLoading || isFetching ? (
    <Box py={4} display="flex" justifyContent="center">
      <CircularProgress />
    </Box>
  ) : (
    <>
      {!votes.length ? (
        <Typography py={4} fontWeight="300">
          {t("govActions.youHaventVotedYet")}
        </Typography>
      ) : !filteredData?.length ? (
        <Typography py={4} fontWeight="300">
          {t("govActions.noResultsForTheSearch")}
        </Typography>
      ) : (
        <Box
          columnGap="20px"
          display="grid"
          gridTemplateColumns={`repeat(auto-fit, minmax(${
            screenWidth < 420 ? "290px" : isMobile ? "324px" : "350px"
          }, 1fr))`}
        >
          {filteredData.map((item) => (
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
