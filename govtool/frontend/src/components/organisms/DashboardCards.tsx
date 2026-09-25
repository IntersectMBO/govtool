import { Box, CircularProgress } from "@mui/material";

import { useCardano, useFeatureFlag } from "@context";
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
  const { isFeatureAvailable } = useFeatureFlag();

  // Whole-feature gate on the delegation surface. The card states, as fact,
  // which DRep this wallet delegates to — and renders the "you have not
  // delegated" call to action when the answer is null. A provider that cannot
  // answer `account.currentDelegation` would therefore tell a delegated user
  // they are undelegated, so the card is hidden rather than shown wrong.
  //
  // This is the NEAREST equivalent: the contract's `drep.delegationTimeline`
  // (who joined, who left, and when) has no surface in this frontend at all.
  const isDelegationStateAvailable = isFeatureAvailable(
    "account.currentDelegation",
  );

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
      {isDelegationStateAvailable && (
        <DelegateDashboardCard
          currentDelegation={currentDelegation}
          delegateTx={pendingTransaction.delegate}
          dRepID={dRepID}
          voter={voter}
          votingPower={votingPower}
        />
      )}
      <DRepDashboardCard
        dRepID={dRepID}
        pendingTransaction={pendingTransaction}
        voter={voter}
      />
      <DirectVoterDashboardCard
        pendingTransaction={pendingTransaction}
        voter={voter}
        votingPower={voter.votingPower}
      />
      <ListGovActionsDashboardCards />
      <ProposeGovActionDashboardCard
        createGovActionTx={pendingTransaction.createGovAction}
      />
    </Box>
  );
};
