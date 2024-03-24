import { useMemo } from "react";
import { Box, Typography, CircularProgress } from "@mui/material";

import { GovernanceVotedOnCard } from "@molecules";
import {
  useGetDRepVotesQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { Slider } from "@organisms";
import { getProposalTypeLabel } from "@/utils/getProposalTypeLabel";
import { getFullGovActionId } from "@/utils";
import { useCardano } from "@/context";

interface DashboardGovernanceActionsVotedOnProps {
  filters: string[];
  searchPhrase?: string;
  sorting: string;
}

export const DashboardGovernanceActionsVotedOn = ({
  filters,
  searchPhrase,
  sorting,
}: DashboardGovernanceActionsVotedOnProps) => {
  const { data, areDRepVotesLoading } = useGetDRepVotesQuery(filters, sorting);
  const { isMobile } = useScreenDimension();
  const { pendingTransaction } = useCardano();
  const { t } = useTranslation();

  const filteredData = useMemo(() => {
    if (data.length && searchPhrase) {
      return data
        .map((entry) => ({
          ...entry,
          actions: entry.actions.filter((action) =>
            getFullGovActionId(action.proposal.txHash, action.proposal.index)
              .toLowerCase()
              .includes(searchPhrase.toLowerCase()),
          ),
        }))
        .filter((entry) => entry.actions.length > 0);
    }
    return data;
  }, [data, searchPhrase, pendingTransaction.vote]);

  return areDRepVotesLoading ? (
    <Box py={4} display="flex" justifyContent="center">
      <CircularProgress />
    </Box>
  ) : (
    <>
      {!data.length ? (
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
                dataLength={item.actions.slice(0, 6).length}
                onDashboard
                data={item.actions.map((action) => (
                  <div
                    className="keen-slider__slide"
                    key={`${action?.proposal.id}${action.vote.vote}`}
                    style={{ overflow: "visible", width: "auto" }}
                  >
                    <GovernanceVotedOnCard
                      votedProposal={action}
                      // TODO: Add data validation
                      isDataMissing={false}
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
