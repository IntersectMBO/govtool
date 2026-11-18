import { ReactNode, useEffect } from "react";
import { Box, Skeleton } from "@mui/material";
import { Link as RouterLink, useNavigate } from "react-router";
import { useTranslation } from "react-i18next";
import { BadgeCheck, Check, KeyRound, Lock, ShieldCheck, UserRoundPlus, Wallet } from "lucide-react";

import { Button } from "@/components/atoms/Button";
import { PageContainer } from "@/components/atoms/PageContainer";
import { CopyButton } from "@/components/organisms/DRepDashboard/parts";
import { StatusPill, WalletRow } from "@/components/organisms/WalletConnect/walletParts";
import { PATHS } from "@/consts/paths";
import { ds, RADIUS, SHADOWS } from "@/consts/designTokens";
import { useCardano } from "@/context/wallet";
import { useGetVoterInfo } from "@/hooks/queries/useGetVoterInfoQuery";
import { useStakeKeyVotingPowers } from "@/hooks/queries/useStakeKeyVotingPower";
import { checkIsWalletConnected } from "@/utils/checkIsWalletConnected";
import { connectedWallet } from "@/utils/connectedWallet";
import { stakeHexToBech32 } from "@/utils/drepDashboard";

const Card = ({ children, testId }: { children: ReactNode; testId: string }) => (
  <Box component="section" data-testid={testId} sx={{ backgroundColor: "#FFFFFF", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, boxShadow: SHADOWS.card, display: "flex", flexDirection: "column", gap: 2, minWidth: 0, p: 3 }}>
    {children}
  </Box>
);

const CardTitle = ({
  children,
  pill,
  icon,
}: {
  children: ReactNode;
  pill?: ReactNode;
  icon?: ReactNode;
}) => (
  <Box sx={{ alignItems: "center", display: "flex", gap: 1.5, justifyContent: "space-between" }}>
    <Box sx={{ alignItems: "center", display: "flex", gap: 1.5, minWidth: 0 }}>
      {icon}
      <Box component="h3" sx={{ color: ds.gray["900"], fontSize: 18, fontWeight: 600, m: 0 }}>{children}</Box>
    </Box>
    {pill}
  </Box>
);

const IconTile = ({ children }: { children: ReactNode }) => (
  <Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.primary["50"], borderRadius: `${RADIUS.md}px`, color: ds.primary["600"], display: "flex", flexShrink: 0, height: 40, justifyContent: "center", width: 40 }}>
    {children}
  </Box>
);

const STEPS = ["wallet", "stakeKey", "review"] as const;

/** Who the connected wallet is in governance: wallet, stake key, voting power, DRep status. */
export const GovernanceProfilePage = () => {
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { stakeKey } = useCardano();
  const { voter } = useGetVoterInfo();
  const { powers, isError } = useStakeKeyVotingPowers(stakeKey ? [stakeKey] : []);

  useEffect(() => {
    if (!checkIsWalletConnected()) navigate(PATHS.home);
  }, []);

  const wallet = connectedWallet();
  const address = stakeKey ? stakeHexToBech32(stakeKey) ?? stakeKey : "";
  const power = stakeKey ? powers[stakeKey] : undefined;

  let status: "notRegistered" | "registered" | "retired" | "direct" = "notRegistered";
  if (voter?.isRegisteredAsDRep) status = "registered";
  else if (voter?.isRegisteredAsSoleVoter) status = "direct";
  else if (voter?.wasRegisteredAsDRep) status = "retired";

  const statusText = {
    notRegistered: { title: "notRegisteredTitle", pill: "notRegisteredPill", body: "notRegisteredBody" },
    registered: { title: "registeredTitle", pill: "registeredPill", body: "registeredBody" },
    retired: { title: "retiredTitle", pill: "retiredPill", body: "retiredBody" },
    direct: { title: "directTitle", pill: "directPill", body: "directBody" },
  }[status];

  return (
    <PageContainer sx={{ display: "flex", flexDirection: "column", gap: 3 }}>
      <Box sx={{ alignItems: { lg: "flex-start" }, display: "flex", flexDirection: { xxs: "column", lg: "row" }, gap: 3, justifyContent: "space-between" }}>
        <Box sx={{ maxWidth: 680 }}>
          <Box component="span" sx={{ alignItems: "center", backgroundColor: ds.primary["50"], borderRadius: `${RADIUS.pill}px`, color: ds.primary["700"], display: "inline-flex", fontSize: 12, fontWeight: 600, gap: 0.75, letterSpacing: "0.3px", px: 1.25, py: 0.5, textTransform: "uppercase" }}>
            <ShieldCheck aria-hidden size={14} />
            {t("profile.badge")}
          </Box>
          <Box component="h1" sx={{ color: ds.gray["900"], fontSize: { xxs: 32, md: 44 }, fontWeight: 500, letterSpacing: "-1px", lineHeight: 1.15, m: 0, mt: 2 }}>
            {t("profile.title")}
          </Box>
          <Box component="p" sx={{ color: ds.gray["600"], fontSize: 16, lineHeight: "24px", m: 0, mt: 1.5 }}>
            {wallet?.name ? t("profile.body", { wallet: wallet.name }) : t("profile.bodyNoWallet")}
          </Box>
        </Box>
        <Box sx={{ backgroundColor: "#FFFFFF", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, maxWidth: { lg: 320 }, p: 2.5 }}>
          <Box sx={{ alignItems: "center", color: ds.gray["900"], display: "flex", fontSize: 15, fontWeight: 600, gap: 1.25 }}>
            <Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.success["50"], borderRadius: `${RADIUS.sm}px`, color: ds.success["700"], display: "flex", height: 32, justifyContent: "center", width: 32 }}>
              <Lock size={16} />
            </Box>
            {t("profile.secureTitle")}
          </Box>
          <Box component="p" sx={{ color: ds.gray["600"], fontSize: 13, lineHeight: "20px", m: 0, mt: 1.5 }}>{t("profile.secureBody")}</Box>
        </Box>
      </Box>

      <Box aria-label={t("profile.steps.label")} component="ol" sx={{ alignItems: "center", backgroundColor: "#FFFFFF", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, display: "flex", flexWrap: "wrap", gap: 2, justifyContent: "space-between", listStyle: "none", m: 0, px: 2.5, py: 1.5 }}>
        {STEPS.map((step, index) => {
          const current = index === STEPS.length - 1;
          return (
            <Box key={step} aria-current={current ? "step" : undefined} component="li" sx={{ alignItems: "center", color: current ? ds.primary["600"] : ds.gray["800"], display: "flex", fontSize: 14, fontWeight: 600, gap: 1 }}>
              {current ? (
                <Box aria-hidden sx={{ backgroundColor: ds.primary["600"], border: "5px solid #FFFFFF", borderRadius: "50%", boxShadow: `0 0 0 2px ${ds.primary["600"]}`, height: 16, width: 16 }} />
              ) : (
                <Check aria-hidden color={ds.success["600"]} size={18} />
              )}
              {t(`profile.steps.${step}`)}
            </Box>
          );
        })}
      </Box>

      <Box sx={{ alignItems: "baseline", display: "flex", flexWrap: "wrap", gap: 1, justifyContent: "space-between" }}>
        <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 20, fontWeight: 600, m: 0 }}>{t("profile.details")}</Box>
        <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{t("profile.detailsHint")}</Box>
      </Box>

      <Box sx={{ alignItems: "start", display: "grid", gap: 2.5, gridTemplateColumns: { xxs: "minmax(0, 1fr)", md: "repeat(2, minmax(0, 1fr))" } }}>
        <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5, minWidth: 0 }}>
          <Card testId="profile-wallet">
            <CardTitle icon={undefined} pill={<StatusPill label={t("profile.wallet.pill")} tone="success" />}>{t("profile.wallet.title")}</CardTitle>
            <Box sx={{ color: ds.gray["600"], fontSize: 13, mt: -1.5 }}>{t("profile.wallet.body")}</Box>
            <WalletRow
              detail={address ? (
                <Box component="span" sx={{ alignItems: "center", display: "inline-flex", gap: 0.5 }}>
                  {`${address.slice(0, 10)}…${address.slice(-8)}`}
                  <CopyButton label={t("profile.stakeKey.label")} value={address} />
                </Box>
              ) : undefined}
              icon={wallet?.icon}
              name={wallet?.name ? t("dashboard.wallet.walletName", { name: wallet.name }) : t("profile.wallet.title")}
            />
          </Card>
          <Card testId="profile-stake-key">
            <CardTitle icon={<IconTile><KeyRound size={18} /></IconTile>} pill={<StatusPill label={t("profile.stakeKey.pill")} tone="primary" />}>
              {t("profile.stakeKey.title")}
            </CardTitle>
            <Box sx={{ backgroundColor: ds.primary["25"], borderRadius: `${RADIUS.md}px`, px: 2, py: 1.5 }}>
              <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{t("profile.stakeKey.label")}</Box>
              <Box data-testid="profile-stake-address" sx={{ color: ds.primary["600"], fontSize: 15, fontWeight: 600, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }} title={address}>
                {address}
              </Box>
            </Box>
            <Box component="p" sx={{ color: ds.gray["600"], fontSize: 13, lineHeight: "20px", m: 0 }}>{t("profile.stakeKey.body")}</Box>
          </Card>
        </Box>
        <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5, minWidth: 0 }}>
          <Card testId="profile-voting-power">
            <CardTitle pill={<Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.primary["50"], borderRadius: `${RADIUS.sm}px`, color: ds.primary["600"], display: "flex", height: 36, justifyContent: "center", width: 36 }}><BadgeCheck size={18} /></Box>}>
              {t("profile.votingPower.title")}
            </CardTitle>
            <Box data-testid="profile-voting-power-value" sx={{ color: ds.primary["600"], fontSize: 34, fontWeight: 500, letterSpacing: "-0.6px" }}>
              {power === undefined && !isError ? <Skeleton width={180} /> : `₳ ${isError ? "–" : (power! / 1_000_000).toLocaleString("en-US", { maximumFractionDigits: 0 })}`}
            </Box>
            <Box component="p" sx={{ color: ds.gray["700"], fontSize: 14, m: 0 }}>{t("profile.votingPower.body")}</Box>
          </Card>
          <Card testId="profile-drep-status">
            {voter ? (
              <>
                <Box sx={{ display: "flex", gap: 1.5 }}>
                  <IconTile><UserRoundPlus size={18} /></IconTile>
                  <Box>
                    <Box component="h3" sx={{ color: ds.gray["900"], fontSize: 18, fontWeight: 600, m: 0 }}>{t(`profile.drep.${statusText.title}`)}</Box>
                    <Box sx={{ mt: 0.75 }}>
                      <StatusPill label={t(`profile.drep.${statusText.pill}`)} tone={status === "registered" ? "success" : status === "retired" ? "warning" : "primary"} />
                    </Box>
                  </Box>
                </Box>
                <Box component="p" sx={{ color: ds.gray["700"], fontSize: 14, lineHeight: "20px", m: 0 }}>{t(`profile.drep.${statusText.body}`)}</Box>
                <Box sx={{ borderTop: `1px solid ${ds.gray["200"]}`, display: "flex", flexDirection: "column", gap: 1, pt: 2 }}>
                  {status === "notRegistered" ? (
                    <Button component={RouterLink} dataTestId="profile-become-drep" sx={{ width: "100%" }} to={PATHS.registerAsdRep}>
                      {t("profile.drep.become")}
                    </Button>
                  ) : (
                    <Button component={RouterLink} dataTestId="profile-open-dashboard" sx={{ width: "100%" }} to={PATHS.dashboard}>
                      {status === "registered" ? t("profile.drep.dashboard") : t("profile.drep.openDashboard")}
                    </Button>
                  )}
                  <Button component={RouterLink} dataTestId="profile-explore" sx={{ width: "100%" }} to={PATHS.dashboardGovernanceActions} variant="text">
                    {t("profile.drep.explore")}
                  </Button>
                </Box>
              </>
            ) : (
              <Skeleton height={180} variant="rounded" />
            )}
          </Card>
        </Box>
      </Box>

      <Box sx={{ alignItems: "flex-start", backgroundColor: ds.primary["25"], border: `1px solid ${ds.primary["100"]}`, borderRadius: `${RADIUS.lg}px`, display: "flex", gap: 1.5, px: 2.5, py: 2 }}>
        <Box aria-hidden sx={{ alignItems: "center", backgroundColor: "#FFFFFF", borderRadius: `${RADIUS.sm}px`, color: ds.primary["600"], display: "flex", flexShrink: 0, height: 32, justifyContent: "center", width: 32 }}>
          <Wallet size={16} />
        </Box>
        <Box>
          <Box sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600 }}>{t("profile.noTxTitle")}</Box>
          <Box sx={{ color: ds.gray["700"], fontSize: 14, lineHeight: "20px", mt: 0.25 }}>{t("profile.noTxBody")}</Box>
        </Box>
      </Box>
    </PageContainer>
  );
};
