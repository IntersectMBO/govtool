import { Box, CircularProgress } from "@mui/material";

import { useAppContext, useCardano, useFeatureFlag } from "@context";
import {
  useGetAdaHolderVotingPowerQuery,
  useGetAdaHolderCurrentDelegationQuery,
  useGetVoterInfo,
  useTranslation,
} from "@hooks";
import { Link as RouterLink, useLocation } from "react-router";
import { UserRoundX } from "lucide-react";
import { Button } from "@/components/atoms/Button";
import { PATHS } from "@/consts/paths";
import { ds, RADIUS, SHADOWS } from "@/consts/designTokens";
import { RetiredBanner } from "./RetiredBanner";
import { UsefulLinks } from "./UsefulLinks";
import { DashboardWalletCard } from "./DashboardWalletCard";
import { DelegateDashboardCard } from "./DashboardCards/DelegateDashboardCard";
import { DRepDashboardCard } from "./DashboardCards/DRepDashboardCard";
import { DirectVoterDashboardCard } from "./DashboardCards/DirectVoterDashboardCard";
import { ListGovActionsDashboardCards } from "./DashboardCards/ListGovActionsDashboardCard";
import { ProposeGovActionDashboardCard } from "./DashboardCards/ProposeGovActionDashboardCard";
import { DRepDashboard } from "./DRepDashboard/DRepDashboard";
import { RetiredDRepDashboard } from "./DRepDashboard/RetiredDRepDashboard";
import { isRetiredDRep } from "@/utils/drepDashboard";

export const DashboardCards = () => {
  const { dRepID, pendingTransaction, stakeKey } = useCardano();
  const { t } = useTranslation();
  // The retire page reports a confirmed retirement in the route state; until
  // the voter record catches up, do not offer to retire again.
  const justRetired =
    (useLocation().state as { retired?: string } | null)?.retired === "directVoter";
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

  const walletCard = (
    <DashboardWalletCard
      currentDelegation={currentDelegation}
      dRepID={dRepID}
      isDelegationKnown={isDelegationStateAvailable}
      stakeKey={stakeKey}
      votingPower={votingPower}
    />
  );

  const actionCards = (
    <>
      {isDelegationStateAvailable && (
      <DelegateDashboardCard
        currentDelegation={currentDelegation}
        delegateTx={pendingTransaction.delegate}
        dRepID={dRepID}
        voter={voter}
        votingPower={votingPower}
      />
            )}
      <DirectVoterDashboardCard
        pendingTransaction={pendingTransaction}
        voter={voter}
        votingPower={voter.votingPower}
      />
      <ProposeGovActionDashboardCard
        createGovActionTx={pendingTransaction.createGovAction}
      />
    </>
  );

  // A registered DRep gets its own tabbed dashboard, and a retired one its
  // retired view; the shared cards become their action cards.
  if (isRetiredDRep(voter, pendingTransaction)) {
    return (
      <>
        <RetiredDRepDashboard actionCards={actionCards} dRepID={dRepID} voter={voter} />
        <UsefulLinks />
      </>
    );
  }

  if (voter.isRegisteredAsDRep) {
    return (
      <>
        <DRepDashboard
          actionCards={actionCards}
          dRepID={dRepID}
          pendingTransaction={pendingTransaction}
          stakeKey={stakeKey}
          voter={voter}
          walletCard={walletCard}
        />
        <UsefulLinks />
      </>
    );
  }

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 3 }}>
      <Box sx={{ alignItems: { md: "flex-start" }, display: "flex", flexDirection: { xxs: "column", md: "row" }, gap: 2, justifyContent: "space-between" }}>
        <Box sx={{ display: "flex", flexDirection: "column", gap: 0.75 }}>
          <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1.25 }}>
            <Box component="h1" sx={{ color: ds.gray["900"], fontSize: { xxs: 26, md: 32 }, fontWeight: 600, letterSpacing: "-0.6px", lineHeight: { xxs: "32px", md: "40px" }, m: 0, overflowWrap: "anywhere" }}>
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
        {role === "directVoter" && !pendingTransaction.retireAsDirectVoter && !justRetired && (
        <Button
          component={RouterLink}
          dataTestId="retire-direct-voter-header-button"
          size="medium"
          startIcon={<UserRoundX aria-hidden size={16} />}
          sx={{
            alignSelf: { xxs: "flex-start", md: "auto" },
            backgroundColor: ds.error["600"],
            boxShadow: SHADOWS.innerError,
            "&:hover": { backgroundColor: ds.error["700"], boxShadow: SHADOWS.innerError },
          }}
          to={PATHS.retireAsDirectVoter}
        >
          {t("directVoter.headerRetire")}
        </Button>
      )}
      </Box>
      <RetiredBanner />
      <Box
        sx={{
          alignItems: "start",
          display: "grid",
          gap: 2.5,
          gridTemplateColumns: { xxs: "minmax(0, 1fr)", xl: "340px minmax(0, 1fr)" },
        }}
      >
        {walletCard}
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
          {/* In the card column, so it lines up with the cards above. */}
          <Box sx={{ mt: 2 }}>
            <UsefulLinks />
          </Box>
        </Box>
      </Box>
    </Box>
  );
};
