import { Box } from "@mui/material";

import { Slider, Typography } from "@govtool/components";
import { useScreenDimension } from "@govtool/hooks";

import { GovernanceActionCard } from "@molecules";
import { getProposalTypeTitle } from "@utils";

type GovernanceActionsToVoteProps = {
  filters: string[];
  sorting: string;
  proposals: { title: string; actions: ActionTypeToDisplay[] }[];
  onDashboard?: boolean;
  searchPhrase?: string;
  pendingTransaction: any;
  noResultsLabel?: string;
  onGovernanceActionCardClick?: () => void;
};

export const GovernanceActionsToVote = ({
  filters,
  onDashboard = true,
  proposals,
  searchPhrase,
  sorting,
  pendingTransaction,
  noResultsLabel,
  onGovernanceActionCardClick,
}: GovernanceActionsToVoteProps) => {
  const { isMobile } = useScreenDimension();
  return (
    <>
      {!proposals.length ? (
        <Typography fontWeight={300} sx={{ py: 4 }}>
          {noResultsLabel}
        </Typography>
      ) : (
        <>
          {proposals?.map((item, index) => (
            <Box key={item.title} pb={2.5}>
              <Slider
                pendingTransaction={pendingTransaction}
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
                      inProgress={
                        onDashboard &&
                        pendingTransaction.vote?.resourceId ===
                          `${action.txHash ?? ""}${action.index ?? ""}`
                      }
                      onClick={onGovernanceActionCardClick}
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
                title={getProposalTypeTitle(item.title)}
              />
              {index < proposals.length - 1 && (
                <Box height={isMobile ? 40 : 52} />
              )}
            </Box>
          ))}
        </>
      )}
    </>
  );
};
