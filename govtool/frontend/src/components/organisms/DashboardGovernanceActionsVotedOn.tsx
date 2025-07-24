import { useMemo } from "react";
import { Box, Typography, CircularProgress } from "@mui/material";

import { useCardano } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";
import { Slider, ValidatedGovernanceVotedOnCard } from "@organisms";
import { getFullGovActionId, getProposalTypeLabel } from "@utils";
import { VotedProposal } from "@/models";

type DashboardGovernanceActionsVotedOnProps = {
  searchPhrase?: string;
  votes: {
    title: string;
    actions: VotedProposal[];
  }[];
  areDRepVotesLoading: boolean;
};

export const DashboardGovernanceActionsVotedOn = ({
  searchPhrase,
  votes,
  areDRepVotesLoading,
}: DashboardGovernanceActionsVotedOnProps) => {
  const { isMobile } = useScreenDimension();
  const { pendingTransaction } = useCardano();
  const { t } = useTranslation();

  // TODO: Filtering here is some kind of craziness. It should be done on the backend.

  const filteredData = useMemo(() => {
    if (!votes?.length) return [];
    if (!searchPhrase) return votes;
    const lowerSearch = searchPhrase.toLowerCase();
    return votes
      .map((entry) => {
        const filteredActions = entry.actions.filter((action) => {
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
        });

        return {
          ...entry,
          actions: filteredActions,
        };
      })
      .filter((entry) => entry.actions.length > 0);
  }, [votes, searchPhrase, pendingTransaction.vote]);

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
      ) : !filteredData?.length ? (
        <Typography py={4} fontWeight="300">
          {t("govActions.noResultsForTheSearch")}
        </Typography>
      ) : (
        <>
          {filteredData?.map((item) => (
            <div key={item.title}>
              <Slider
                key={item.title}
                title={getProposalTypeLabel(item.title)}
                navigateKey={item.title}
                searchPhrase={searchPhrase}
                dataLength={item.actions.slice(0, 6)?.length}
                onDashboard
                data={item.actions.map((action) => (
                  <div
                    className="keen-slider__slide"
                    key={`${action?.proposal.id}${action.vote?.vote}`}
                    style={{ overflow: "visible", width: "auto" }}
                  >
                    <ValidatedGovernanceVotedOnCard
                      votedProposal={action}
                      inProgress={
                        pendingTransaction.vote?.resourceId ===
                        action.proposal.txHash + action.proposal.index
                      }
                    />
                  </div>
                ))}
              />
              <Box height={isMobile ? 50 : 72} />
            </div>
          ))}
        </>
      )}
    </>
  );
};
