import { useMemo } from "react";
import { Box, Typography, CircularProgress } from "@mui/material";

import { useCardano } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";
import { GovernanceVotedOnCard } from "@molecules";
import { Slider } from "@organisms";
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
    if (votes.length && searchPhrase) {
      return votes
        .map((entry) => ({
          ...entry,
          actions: entry.actions.filter((action) =>
            getFullGovActionId(action.proposal.txHash, action.proposal.index)
              .toLowerCase()
              .includes(searchPhrase.toLowerCase()),
          ),
        }))
        .filter((entry) => entry.actions?.length > 0);
    }
    return votes;
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
                    <GovernanceVotedOnCard
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
