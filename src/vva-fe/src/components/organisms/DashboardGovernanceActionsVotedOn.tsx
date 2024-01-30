import { useMemo } from "react";
import { Box, Typography, CircularProgress } from "@mui/material";

import { GovernanceVotedOnCard } from "@molecules";
import { Slider } from ".";
import {
  useGetDRepVotesQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
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
  const { data, dRepVotesAreLoading } = useGetDRepVotesQuery(filters, sorting);
  const { isMobile } = useScreenDimension();
  const { voteTransaction } = useCardano();
  const { t } = useTranslation();

  const filteredData = useMemo(() => {
    if (data.length && searchPhrase) {
      return data
        .map((entry) => {
          return {
            ...entry,
            actions: entry.actions.filter((action) => {
              return getFullGovActionId(
                action.proposal.txHash,
                action.proposal.index
              )
                .toLowerCase()
                .includes(searchPhrase.toLowerCase());
            }),
          };
        })
        .filter((entry) => entry.actions.length > 0);
    } else {
      return data;
    }
  }, [data, searchPhrase, voteTransaction.transactionHash]);

  return dRepVotesAreLoading ? (
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
          {t("govActions.noResults")}
        </Typography>
      ) : (
        <>
          {filteredData?.map((item) => (
            <div key={item.title}>
              <Slider
                key={item.title}
                isShowAll={false}
                title={getProposalTypeLabel(item.title)}
                navigateKey={item.title}
                searchPhrase={searchPhrase}
                data={item.actions.map((item) => {
                  return (
                    <div
                      className="keen-slider__slide"
                      key={`${item?.proposal.id}${item.vote.vote}`}
                      style={{ overflow: "visible", width: "auto" }}
                    >
                      <GovernanceVotedOnCard
                        votedProposal={item}
                        searchPhrase={searchPhrase}
                        inProgress={
                          voteTransaction.proposalId ===
                          item.proposal.txHash + item.proposal.index
                        }
                      />
                    </div>
                  );
                })}
              />
              <Box height={isMobile ? 50 : 72} />
            </div>
          ))}
        </>
      )}
    </>
  );
};
