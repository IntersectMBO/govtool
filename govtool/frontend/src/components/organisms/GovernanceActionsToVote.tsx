/* eslint-disable no-unsafe-optional-chaining */
import { useMemo } from "react";
import { useNavigate, generatePath } from "react-router-dom";
import { Box, CircularProgress } from "@mui/material";

import { Typography } from "@atoms";
import {
  useGetProposalsQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { GovernanceActionCard } from "@molecules";
import { GOVERNANCE_ACTIONS_FILTERS, PATHS } from "@consts";
import { useCardano } from "@context";
import { getProposalTypeLabel, getFullGovActionId, openInNewTab } from "@utils";
import { Slider } from "./Slider";

type GovernanceActionsToVoteProps = {
  filters: string[];
  onDashboard?: boolean;
  searchPhrase?: string;
  sorting: string;
};

const defaultCategories = GOVERNANCE_ACTIONS_FILTERS.map(
  (category) => category.key,
);

export const GovernanceActionsToVote = ({
  filters,
  onDashboard = true,
  searchPhrase = "",
  sorting,
}: GovernanceActionsToVoteProps) => {
  const { voteTransaction } = useCardano();
  const navigate = useNavigate();
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  const queryFilters = filters.length > 0 ? filters : defaultCategories;

  const { proposals, isProposalsLoading } = useGetProposalsQuery({
    filters: queryFilters,
    sorting,
  });

  const groupedByType = (data?: ActionType[]) =>
    data?.reduce((groups, item) => {
      const itemType = item.type;

      // TODO: Provide better typing for groups
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-expect-error
      if (!groups[itemType]) {
        // eslint-disable-next-line @typescript-eslint/ban-ts-comment
        // @ts-expect-error
        groups[itemType] = {
          title: itemType,
          actions: [],
        };
      }
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-expect-error
      groups[itemType].actions.push(item);

      return groups;
    }, {});

  const mappedData = useMemo(() => {
    const groupedData = groupedByType(
      proposals?.filter((i) =>
        getFullGovActionId(i.txHash, i.index)
          .toLowerCase()
          .includes(searchPhrase.toLowerCase()),
      ),
    );
    return Object.values(groupedData ?? []) as ToVoteDataType;
  }, [proposals, searchPhrase]);

  return !isProposalsLoading ? (
    <>
      {!mappedData.length ? (
        <Typography fontWeight={300} sx={{ py: 4 }}>
          {t("govActions.noResultsForTheSearch")}
        </Typography>
      ) : (
        <>
          {mappedData?.map((item, index) => (
            <Box key={item.title} pb={2.5}>
              <Slider
                data={item.actions.slice(0, 6).map((action) => (
                  <div
                    className="keen-slider__slide"
                    key={action.id}
                    style={{
                      overflow: "visible",
                      width: "auto",
                    }}
                  >
                    <GovernanceActionCard
                      {...action}
                      txHash={action.txHash}
                      index={action.index}
                      inProgress={
                        onDashboard &&
                        voteTransaction?.proposalId ===
                          action?.txHash + action?.index
                      }
                      // eslint-disable-next-line no-confusing-arrow
                      onClick={() =>
                        onDashboard &&
                        voteTransaction?.proposalId ===
                          action?.txHash + action?.index
                          ? openInNewTab(
                              "https://adanordic.com/latest_transactions",
                            )
                          : navigate(
                              onDashboard
                                ? generatePath(
                                    PATHS.dashboardGovernanceActionsAction,
                                    {
                                      proposalId: getFullGovActionId(
                                        action.txHash,
                                        action.index,
                                      ),
                                    },
                                  )
                                : PATHS.governanceActionsAction.replace(
                                    ":proposalId",
                                    getFullGovActionId(
                                      action.txHash,
                                      action.index,
                                    ),
                                  ),
                              {
                                state: { ...action },
                              },
                            )
                      }
                    />
                  </div>
                ))}
                dataLength={item.actions.slice(0, 6).length}
                filters={filters}
                navigateKey={item.title}
                notSlicedDataLength={item.actions.length}
                onDashboard={onDashboard}
                searchPhrase={searchPhrase}
                sorting={sorting}
                title={getProposalTypeLabel(item.title)}
              />
              {index < mappedData.length - 1 && (
                <Box height={isMobile ? 40 : 52} />
              )}
            </Box>
          ))}
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
    >
      <CircularProgress />
    </Box>
  );
};
