import { Box, CircularProgress } from "@mui/material";

import { useCardano } from "@context";
import {
  useGetAdaHolderVotingPowerQuery,
  useScreenDimension,
  useGetAdaHolderCurrentDelegationQuery,
  useGetVoterInfo,
} from "@hooks";
import { PROTOCOL_PARAMS_KEY, getItemFromLocalStorage } from "@utils";
import { DelegateDashboardCard } from "./DashboardCards/DelegateDashboardCard";
import { DRepDashboardCard } from "./DashboardCards/DRepDashboardCard";
import { DirectVoterDashboardCard } from "./DashboardCards/DirectVoterDashboardCard";
import { ListGovActionsDashboardCards } from "./DashboardCards/ListGovActionsDashboardCard";
import { ProposeGovActionDashboardCard } from "./DashboardCards/ProposeGovActionDashboardCard";

const protocolParams = getItemFromLocalStorage(PROTOCOL_PARAMS_KEY);

export const DashboardCards = () => {
  const { dRepID, dRepIDBech32, pendingTransaction, stakeKey } = useCardano();
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
          height: "100vh",
          justifyContent: "center",
        }}
      >
        <CircularProgress />
      </Box>
    );
  }

  return (
    <Box flex={1}>
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
          px: screenWidth < 640 ? 2 : 5,
          py: 3,
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
          dRepIDBech32={dRepIDBech32}
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
          deposit={protocolParams?.gov_action_deposit}
          votingPower={votingPower}
        />
      </Box>
    </Box>
  );
};
