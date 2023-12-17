import { useMemo } from "react";
import { useNavigate, generatePath } from "react-router-dom";
import { Box, CircularProgress } from "@mui/material";

import { Slider } from "./Slider";

import { Typography } from "@atoms";
import {
  useGetDRepVotesQuery,
  useGetProposalsQuery,
  useScreenDimension,
} from "@hooks";
import { GovernanceActionCard } from "@molecules";
import { GOVERNANCE_ACTIONS_FILTERS, PATHS } from "@consts";
import { useCardano } from "@context";
import { getProposalTypeLabel, getFullGovActionId, openInNewTab } from "@utils";

type GovernanceActionsToVoteProps = {
  filters: string[];
  onDashboard?: boolean;
  searchPhrase?: string;
  sorting: string;
};

const defaultCategories = GOVERNANCE_ACTIONS_FILTERS.map(
  (category) => category.key
);

export const GovernanceActionsToVote = ({
  filters,
  onDashboard = true,
  searchPhrase = "",
  sorting,
}: GovernanceActionsToVoteProps) => {
  const { dRep, voteTransaction } = useCardano();
  const { data: dRepVotes, dRepVotesAreLoading } = useGetDRepVotesQuery([], "");
  const navigate = useNavigate();
  const { isMobile } = useScreenDimension();

  const queryFilters = filters.length > 0 ? filters : defaultCategories;

  const { proposals, isLoading } = useGetProposalsQuery(queryFilters, sorting);

  const groupedByType = (data?: ActionType[]) => {
    return data?.reduce((groups, item) => {
      const itemType = item.type;

      if (!groups[itemType]) {
        groups[itemType] = {
          title: itemType,
          actions: [],
        };
      }

      groups[itemType].actions.push(item);

      return groups;
    }, {});
  };

  const mappedData = useMemo(() => {
    if (onDashboard && dRep?.isRegistered && dRepVotes) {
      const filteredBySearchPhrase = proposals?.filter((i) =>
        getFullGovActionId(i.txHash, i.index)
          .toLowerCase()
          .includes(searchPhrase.toLowerCase())
      );
      const filteredData = filteredBySearchPhrase?.filter((i) => {
        return !dRepVotes
          .flatMap((item) => item.actions.map((item) => item.proposal.id))
          .includes(i.id);
      });
      const groupedData = groupedByType(filteredData);
      return Object.values(groupedData ?? []) as ToVoteDataType;
    }
    const groupedData = groupedByType(
      proposals?.filter((i) =>
        getFullGovActionId(i.txHash, i.index)
          .toLowerCase()
          .includes(searchPhrase.toLowerCase())
      )
    );
    return Object.values(groupedData ?? []) as ToVoteDataType;
  }, [
    proposals,
    onDashboard,
    dRep?.isRegistered,
    dRepVotes,
    searchPhrase,
    voteTransaction.proposalId,
  ]);

  return !isLoading && !dRepVotesAreLoading ? (
    <>
      {!mappedData.length ? (
        <Typography fontWeight={300} sx={{ py: 4 }}>
          No results for the search
        </Typography>
      ) : (
        <>
          {mappedData?.map((item, index) => {
            return (
              <Box key={item.title} pb={2.5}>
                <Slider
                  data={item.actions.slice(0, 6).map((item) => {
                    return (
                      <div
                        className="keen-slider__slide"
                        key={item.id}
                        style={{
                          overflow: "visible",
                          width: "auto",
                        }}
                      >
                        <GovernanceActionCard
                          {...item}
                          txHash={item.txHash}
                          index={item.index}
                          inProgress={
                            onDashboard &&
                            voteTransaction?.proposalId ===
                              item?.txHash + item?.index
                          }
                          onClick={() =>
                            onDashboard &&
                            voteTransaction?.proposalId ===
                              item?.txHash + item?.index
                              ? openInNewTab(
                                  "https://adanordic.com/latest_transactions"
                                )
                              : navigate(
                                  onDashboard
                                    ? generatePath(
                                        PATHS.dashboard_governance_actions_action,
                                        {
                                          proposalId: getFullGovActionId(
                                            item.txHash,
                                            item.index
                                          ),
                                        }
                                      )
                                    : PATHS.governance_actions_action.replace(
                                        ":proposalId",
                                        getFullGovActionId(
                                          item.txHash,
                                          item.index
                                        )
                                      ),
                                  {
                                    state: { ...item },
                                  }
                                )
                          }
                        />
                      </div>
                    );
                  })}
                  dataLength={item.actions.slice(0, 6).length}
                  notSlicedDataLength={item.actions.length}
                  filters={filters}
                  onDashboard={onDashboard}
                  searchPhrase={searchPhrase}
                  sorting={sorting}
                  title={getProposalTypeLabel(item.title)}
                  navigateKey={item.title}
                />
                {index < mappedData.length - 1 && (
                  <Box height={isMobile ? 40 : 52} />
                )}
              </Box>
            );
          })}
        </>
      )}
    </>
  ) : (
    <Box
      alignItems="center"
      display="flex"
      flex={1}
      height="100%"
      justifyContent="center"
      py={4}
    >
      <CircularProgress />
    </Box>
  );
};
