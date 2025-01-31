import { Box, CircularProgress } from "@mui/material";

import { useCardano } from "@context";
import {
  useGetAdaHolderVotingPowerQuery,
  useScreenDimension,
  useGetAdaHolderCurrentDelegationQuery,
  useGetVoterInfo,
} from "@hooks";
import { DelegateDashboardCard } from "./DashboardCards/DelegateDashboardCard";
import { DRepDashboardCard } from "./DashboardCards/DRepDashboardCard";
import { DirectVoterDashboardCard } from "./DashboardCards/DirectVoterDashboardCard";
import { ListGovActionsDashboardCards } from "./DashboardCards/ListGovActionsDashboardCard";
import { ProposeGovActionDashboardCard } from "./DashboardCards/ProposeGovActionDashboardCard";

export const DashboardCards = () => {
  const { dRepID, pendingTransaction, stakeKey } = useCardano();
  const { screenWidth } = useScreenDimension();

  const { currentDelegation } = useGetAdaHolderCurrentDelegationQuery(stakeKey);
  const { votingPower } = useGetAdaHolderVotingPowerQuery(stakeKey);
  const { voter } = useGetVoterInfo();

  if (
    currentDelegation === undefined ||
    votingPower === undefined ||
    voter === undefined
  ) {
    return (
      <Box
        sx={{
          alignItems: "center",
          display: "flex",
          flex: 1,
          minHeight: "calc(100vh - 175px)",
          justifyContent: "center",
        }}
      >
        <CircularProgress />
      </Box>
    );
  }

  return (
    <Box
      sx={{
        columnGap: 3,
        display: "grid",
        gridTemplateColumns:
          screenWidth < 1280
            ? "repeat(1, minmax(300px, 530px))"
            : screenWidth >= 1728
            ? "repeat(3, minmax(300px, 570px))"
            : "repeat(2, minmax(300px, 530px))",
        justifyContent: screenWidth < 1024 ? "center" : "flex-start",
        rowGap: 3,
      }}
    >
      <DelegateDashboardCard
        currentDelegation={currentDelegation}
        delegateTx={pendingTransaction.delegate}
        dRepID={dRepID}
        voter={voter}
        votingPower={votingPower}
      />
      <DRepDashboardCard
        dRepID={dRepID}
        pendingTransaction={pendingTransaction}
        voter={voter}
      />
      <DirectVoterDashboardCard
        pendingTransaction={pendingTransaction}
        voter={voter}
        votingPower={votingPower}
      />
      <ListGovActionsDashboardCards />
      <ProposeGovActionDashboardCard
        createGovActionTx={pendingTransaction.createGovAction}
      />
    </Box>
  );
};
