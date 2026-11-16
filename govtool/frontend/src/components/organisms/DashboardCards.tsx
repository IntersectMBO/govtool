import { Box, CircularProgress } from "@mui/material";

import { useAppContext, useCardano, useFeatureFlag } from "@context";
import {
  useGetAdaHolderVotingPowerQuery,
  useGetAdaHolderCurrentDelegationQuery,
  useGetVoterInfo,
  useTranslation,
} from "@hooks";
import { ds, RADIUS } from "@/consts/designTokens";
import { DashboardWalletCard } from "./DashboardWalletCard";
import { DelegateDashboardCard } from "./DashboardCards/DelegateDashboardCard";
import { DRepDashboardCard } from "./DashboardCards/DRepDashboardCard";
import { DirectVoterDashboardCard } from "./DashboardCards/DirectVoterDashboardCard";
import { ListGovActionsDashboardCards } from "./DashboardCards/ListGovActionsDashboardCard";
import { ProposeGovActionDashboardCard } from "./DashboardCards/ProposeGovActionDashboardCard";

export const DashboardCards = () => {
  const { dRepID, pendingTransaction, stakeKey } = useCardano();
  const { t } = useTranslation();
  const { epochParams } = useAppContext();
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

  const role = voter.isRegisteredAsDRep
    ? "dRep"
    : voter.isRegisteredAsSoleVoter
      ? "directVoter"
      : "notRegistered";

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 3 }}>
      <Box sx={{ display: "flex", flexDirection: "column", gap: 0.75 }}>
        <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1.25 }}>
          <Box component="h1" sx={{ color: ds.gray["900"], fontSize: { xxs: 26, md: 32 }, fontWeight: 600, letterSpacing: "-0.6px", lineHeight: { xxs: "32px", md: "40px" }, m: 0 }}>
            {voter.givenName
              ? t("dashboard.welcomeName", { name: voter.givenName })
              : t("dashboard.welcome")}
          </Box>
          <Box
            component="span"
            data-testid="dashboard-role-pill"
            sx={{ backgroundColor: role === "notRegistered" ? ds.gray["100"] : ds.primary["50"], borderRadius: `${RADIUS.xs}px`, color: role === "notRegistered" ? ds.gray["700"] : ds.primary["700"], fontSize: 11, fontWeight: 600, letterSpacing: "0.3px", lineHeight: "18px", px: 0.75, textTransform: "uppercase" }}
          >
            {t(`dashboard.role.${role}`)}
          </Box>
        </Box>
        {typeof epochParams?.epoch_no === "number" && (
          <Box sx={{ color: ds.gray["500"], fontSize: 13 }}>
            {t("dashboard.currentEpoch", { epoch: epochParams.epoch_no })}
          </Box>
        )}
      </Box>
      <Box
        sx={{
          alignItems: "start",
          display: "grid",
          gap: 2.5,
          gridTemplateColumns: { xxs: "minmax(0, 1fr)", lg: "360px minmax(0, 1fr)" },
        }}
      >
        <DashboardWalletCard
          currentDelegation={currentDelegation}
          dRepID={dRepID}
          isDelegationKnown={isDelegationStateAvailable}
          stakeKey={stakeKey}
          votingPower={votingPower}
        />
        <Box sx={{ display: "flex", flexDirection: "column", gap: 2, minWidth: 0 }}>
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
      </Box>
    </Box>
  );
};
