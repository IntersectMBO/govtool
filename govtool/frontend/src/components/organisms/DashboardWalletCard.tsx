import { Box, ButtonBase } from "@mui/material";
import { Link } from "react-router";
import { ArrowRight, Copy, Wallet } from "lucide-react";

import { PATHS } from "@consts";
import { useSnackbar } from "@context";
import { useTranslation } from "@hooks";
import type { CurrentDelegation } from "@models";
import { correctVoteAdaFormat } from "@/utils/adaFormat";
import { connectedWallet } from "@/utils/connectedWallet";
import { stakeHexToBech32 } from "@/utils/drepDashboard";
import { AutomatedVotingOptionCurrentDelegation } from "@/types/automatedVotingOptions";
import { ds, RADIUS, SHADOWS } from "@/consts/designTokens";

type Props = {
  stakeKey?: string;
  votingPower?: number;
  currentDelegation: CurrentDelegation;
  dRepID?: string;
  /** False when the backend cannot say who the wallet delegates to. */
  isDelegationKnown: boolean;
};

const short = (value: string, tail = 6) => `${value.slice(0, 10)}…${value.slice(-tail)}`;

/** The connected wallet: name, stake key, voting power and delegation. */
export const DashboardWalletCard = ({
  stakeKey,
  votingPower,
  currentDelegation,
  dRepID,
  isDelegationKnown,
}: Props) => {
  const { t } = useTranslation();
  const { addSuccessAlert } = useSnackbar();
  const wallet = connectedWallet();
  // The design shows the stake address as stake1…; the wallet hands over hex.
  const stakeAddress = stakeKey ? stakeHexToBech32(stakeKey) ?? stakeKey : undefined;
  const view = currentDelegation?.dRepView;

  const delegation = (() => {
    if (!view) return undefined;
    if (view === AutomatedVotingOptionCurrentDelegation.drep_always_abstain)
      return t("dashboard.wallet.abstain");
    if (view === AutomatedVotingOptionCurrentDelegation.drep_always_no_confidence)
      return t("dashboard.wallet.noConfidence");
    if (currentDelegation?.dRepHash && currentDelegation.dRepHash === dRepID)
      return t("dashboard.wallet.yourself");
    return short(view);
  })();

  return (
    <Box
      component="section"
      data-testid="dashboard-wallet-card"
      sx={{
        backgroundColor: "#FFFFFF",
        border: `1px solid ${ds.gray["200"]}`,
        borderRadius: `${RADIUS.lg}px`,
        borderTop: `3px solid ${ds.primary["300"]}`,
        boxShadow: SHADOWS.card,
        display: "flex",
        flexDirection: "column",
        gap: 2,
        p: { xxs: 2, md: 2.75 },
      }}
    >
      <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600, m: 0 }}>
        {t("dashboard.wallet.title")}
      </Box>
      <Box sx={{ alignItems: "center", display: "flex", gap: 1.5, minWidth: 0 }}>
        <Box sx={{ alignItems: "center", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.md}px`, color: ds.primary["600"], display: "flex", flexShrink: 0, height: 40, justifyContent: "center", width: 40 }}>
          {wallet?.icon ? (
            <Box alt="" component="img" data-testid="dashboard-wallet-icon" src={wallet.icon} sx={{ height: 24, objectFit: "contain", width: 24 }} />
          ) : (
            <Wallet aria-hidden size={18} />
          )}
        </Box>
        <Box sx={{ minWidth: 0 }}>
          <Box sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 500 }}>
            {t("dashboard.wallet.walletName", { name: wallet?.name ?? "" })}
          </Box>
          {stakeAddress && (
            <ButtonBase
              aria-label={t("dashboard.wallet.copyStakeKey", { key: stakeAddress })}
              data-testid="dashboard-wallet-stake-key"
              onClick={() => {
                navigator.clipboard.writeText(stakeAddress);
                addSuccessAlert(t("alerts.copiedToClipboard"));
              }}
              sx={{ borderRadius: `${RADIUS.xs}px`, color: ds.gray["600"], fontFamily: "inherit", fontSize: 13, gap: 0.5, "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` } }}
            >
              {short(stakeAddress, 8)}
              <Copy aria-hidden size={13} />
            </ButtonBase>
          )}
        </Box>
      </Box>
      <Box sx={{ borderTop: `1px solid ${ds.gray["100"]}` }} />
      <Box>
        <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{t("dashboard.wallet.votingPower")}</Box>
        <Box data-testid="dashboard-wallet-voting-power" sx={{ color: ds.gray["900"], fontSize: 26, fontWeight: 600, letterSpacing: "-0.5px" }}>
          {votingPower === undefined ? "—" : `₳\u00a0${correctVoteAdaFormat(votingPower)}`}
        </Box>
      </Box>
      {isDelegationKnown && (
        <>
          <Box sx={{ borderTop: `1px solid ${ds.gray["100"]}` }} />
          <Box sx={{ alignItems: "flex-end", display: "flex", gap: 2, justifyContent: "space-between" }}>
            <Box sx={{ minWidth: 0 }}>
              <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{t("dashboard.wallet.delegation")}</Box>
              <Box data-testid="dashboard-wallet-delegation" sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                {delegation ?? t("dashboard.wallet.notDelegated")}
              </Box>
            </Box>
            <Box
              component={Link}
              to={PATHS.dashboardDRepDirectory}
              sx={{ alignItems: "center", color: ds.orange["700"], display: "inline-flex", flexShrink: 0, fontSize: 13, fontWeight: 600, gap: 0.5, textDecoration: "none", "&:hover": { textDecoration: "underline" } }}
            >
              {delegation ? t("dashboard.wallet.change") : t("dashboard.wallet.delegateNow")}
              <ArrowRight aria-hidden size={14} />
            </Box>
          </Box>
        </>
      )}
    </Box>
  );
};
