import { ReactNode } from "react";
import { Box } from "@mui/material";
import { Link as RouterLink } from "react-router";
import { useTranslation } from "react-i18next";
import { CircleCheck, TriangleAlert } from "lucide-react";

import { Button } from "@/components/atoms/Button";
import { PATHS } from "@/consts/paths";
import { ds, PILL_TONES, RADIUS, SHADOWS } from "@/consts/designTokens";
import { useAppContext } from "@/context/appContext";
import { useGetDRepDetailsQuery } from "@/hooks/queries/useGetDrepDetailsQuery";
import {
  useDRepDashboardVotes,
  useDRepHistory,
  useDRepVotingPowerHistory,
} from "@/hooks/queries/useDRepDashboard";
import type { VoterInfo } from "@/models";
import { tenureMonths } from "@/utils/drepDashboard";

import { twoColumns } from "./OverviewTab";
import { formatAda, formatLongDate, formatShortDate, Panel } from "./parts";

const Muted = ({ label, value }: { label: string; value?: string | null }) =>
  (value ? (
    <Box sx={{ minWidth: 0 }}>
      <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{label}</Box>
      <Box sx={{ color: ds.gray["700"], fontSize: 14, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }} title={value}>
        {value}
      </Box>
    </Box>
  ) : null);

const Tile = ({ label, value, testId }: { label: string; value: ReactNode; testId: string }) => (
  <Box data-testid={testId} sx={{ backgroundColor: ds.gray["50"], borderRadius: `${RADIUS.md}px`, flex: 1, minWidth: 160, p: 2 }}>
    <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{label}</Box>
    <Box sx={{ color: ds.gray["900"], fontSize: 20, fontWeight: 600, mt: 0.5 }}>{value}</Box>
  </Box>
);

type Props = {
  voter: VoterInfo;
  dRepID: string;
  /** Delegate, direct voter and propose cards, built by DashboardCards. */
  actionCards: ReactNode;
};

/** The dashboard of a wallet whose DRep registration is retired. */
export const RetiredDRepDashboard = ({ voter, dRepID, actionCards }: Props) => {
  const { t } = useTranslation();
  const { epochParams } = useAppContext();
  const { dRep } = useGetDRepDetailsQuery(dRepID);
  const { votes } = useDRepDashboardVotes(dRepID);
  const history = useDRepHistory(dRepID, 1, 100, "desc");
  const growth = useDRepVotingPowerHistory(dRepID);

  const name = voter.givenName || dRep?.givenName;
  const cast = (votes ?? []).filter((row) => row.vote);
  const archived = [...cast]
    .sort((a, b) => new Date(b.vote?.date ?? 0).getTime() - new Date(a.vote?.date ?? 0).getTime())
    .slice(0, 3);
  const retiredAt = history.isServed
    ? history.data?.elements.find((event) => event.kind === "retired")?.at.time
    : undefined;
  const months = tenureMonths(dRep?.latestRegistrationDate, retiredAt);
  const finalPower = dRep?.votingPower ?? voter.votingPower ?? undefined;
  const powerText = (lovelace?: number | null) =>
    (lovelace === undefined || lovelace === null ? "–" : `₳ ${formatAda(lovelace, 0)}`);
  const peak = growth.isServed && growth.data?.length
    ? Math.max(...growth.data.map((point) => Number(point.votingPower) || 0))
    : undefined;
  // Registering again costs today's deposit; the refund was the deposit paid.
  const nextDeposit = epochParams?.drep_deposit
    ? formatAda(epochParams.drep_deposit, 0)
    : undefined;
  const refunded = voter.deposit ? formatAda(voter.deposit, 0) : undefined;
  const field = (key: "objectives" | "motivations" | "qualifications") =>
    voter[key] ?? dRep?.[key] ?? null;
  const currentEpoch = typeof epochParams?.epoch_no === "number" ? epochParams.epoch_no : undefined;

  const reactivate = (testId: string, fullWidth = false) => (
    <Button
      component={RouterLink}
      dataTestId={testId}
      size="medium"
      color="secondary"
      sx={fullWidth ? { width: "100%" } : undefined}
      to={PATHS.registerAsdRep}
    >
      {t("drepDashboard.retired.reactivate")}
    </Button>
  );

  return (
    <Box data-testid="retired-drep-dashboard" sx={{ display: "flex", flexDirection: "column", gap: 3 }}>
      <Box sx={{ alignItems: { md: "flex-start" }, display: "flex", flexDirection: { xxs: "column", md: "row" }, gap: 2, justifyContent: "space-between" }}>
        <Box sx={{ display: "flex", flexDirection: "column", gap: 1 }}>
          <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1.25 }}>
            <Box component="h1" sx={{ color: ds.gray["900"], fontSize: { xxs: 26, md: 30 }, fontWeight: 600, letterSpacing: "-0.6px", m: 0 }}>
              {name ? t("drepDashboard.welcome", { name }) : t("drepDashboard.welcomeAnonymous")}
            </Box>
            <Box
              component="span"
              data-testid="dashboard-role-pill"
              sx={{ backgroundColor: PILL_TONES.neutral.bg, border: `1px solid ${ds.gray["300"]}`, borderRadius: `${RADIUS.pill}px`, color: PILL_TONES.neutral.text, fontSize: 12, fontWeight: 600, letterSpacing: "0.3px", px: 1, textTransform: "uppercase" }}
            >
              {t("drepDashboard.retired.role")}
            </Box>
          </Box>
          <Box sx={{ color: ds.gray["500"], display: "flex", flexWrap: "wrap", fontSize: 13, gap: 1 }}>
            {dRep?.latestRegistrationDate && (
              <span>
                {t("drepDashboard.registered")}{" "}
                <Box component="span" sx={{ color: ds.gray["800"], fontWeight: 500 }}>{formatLongDate(dRep.latestRegistrationDate)}</Box>
              </span>
            )}
            {currentEpoch !== undefined && (
              <span>
                {t("drepDashboard.currentEpoch")}{" "}
                <Box component="span" sx={{ color: ds.gray["800"], fontWeight: 500 }}>{currentEpoch}</Box>
              </span>
            )}
          </Box>
        </Box>
        {reactivate("reactivate-drep-header-button")}
      </Box>

      <Box
        data-testid="retired-drep-banner"
        role="status"
        sx={{ alignItems: "center", backgroundColor: ds.warning["50"], border: `1px solid ${ds.warning["100"]}`, borderLeft: `3px solid ${ds.warning["500"]}`, borderRadius: `${RADIUS.md}px`, color: ds.warning["700"], display: "flex", fontSize: 14, fontWeight: 500, gap: 1.25, px: 2, py: 1.5 }}
      >
        <TriangleAlert aria-hidden size={18} style={{ flexShrink: 0 }} />
        {t("drepDashboard.retired.banner")}
      </Box>

      <Box sx={twoColumns}>
        <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5, minWidth: 0 }}>
          <Box component="section" data-testid="retired-return-card" sx={{ backgroundColor: ds.orange["50"], border: `1px solid ${ds.orange["300"]}`, borderRadius: `${RADIUS.lg}px`, display: "flex", flexDirection: "column", gap: 2, p: 3 }}>
            <Box component="h2" sx={{ color: ds.orange["750"], fontSize: 18, fontWeight: 600, m: 0 }}>{t("drepDashboard.retired.returnTitle")}</Box>
            <Box component="p" sx={{ color: ds.orange["750"], fontSize: 14, lineHeight: "20px", m: 0 }}>
              {nextDeposit
                ? t("drepDashboard.retired.returnBody", { deposit: nextDeposit })
                : t("drepDashboard.retired.returnBodyNoDeposit")}
            </Box>
            {reactivate("register-button", true)}
          </Box>
          <Panel title={t("drepDashboard.retired.finalStats")} titleId="retired-final-title">
            <Box>
              <Box data-testid="retired-final-power" sx={{ color: ds.gray["900"], fontSize: 30, fontWeight: 600, letterSpacing: "-0.6px" }}>
                {powerText(finalPower)}
              </Box>
              <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{t("drepDashboard.retired.finalPower")}</Box>
            </Box>
          </Panel>
          <Panel title={t("drepDashboard.retired.links")} titleId="retired-links-title">
            <Muted label={t("drepDashboard.overview.identity.drepId")} value={dRep?.view} />
            <Muted label={t("drepDashboard.overview.identity.paymentAddress")} value={voter.paymentAddress} />
            <Muted label={t("drepDashboard.overview.identity.metadataUrl")} value={voter.url} />
            <Muted label={t("drepDashboard.overview.identity.metadataHash")} value={dRep?.metadataHash} />
            {[...(dRep?.linkReferences ?? []), ...(dRep?.identityReferences ?? [])].map((ref) => (
              <Muted key={`${ref.label}-${ref.uri}`} label={ref.label} value={ref.uri} />
            ))}
          </Panel>
        </Box>

        <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5, minWidth: 0 }}>
          <Box sx={{ alignItems: "baseline", display: "flex", flexWrap: "wrap", gap: 1, justifyContent: "space-between" }}>
            <Box component="h2" sx={{ color: ds.gray["700"], fontSize: 18, fontWeight: 600, m: 0 }}>{t("drepDashboard.retired.profileTitle")}</Box>
            <Box sx={{ color: ds.gray["600"], fontSize: 12, fontWeight: 600, letterSpacing: "0.4px", textTransform: "uppercase" }}>{t("drepDashboard.retired.record")}</Box>
          </Box>
          <Box component="section" sx={{ backgroundColor: ds.gray["50"], border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, display: "flex", flexDirection: "column", gap: 2, p: 3 }}>
            <Box component="h3" sx={{ color: ds.gray["600"], fontSize: 18, fontWeight: 600, m: 0 }}>{t("drepDashboard.overview.details.title")}</Box>
            {(["objectives", "motivations", "qualifications"] as const).map((key) => (
              <Box key={key}>
                <Box component="h4" sx={{ color: ds.gray["700"], fontSize: 16, fontWeight: 600, m: 0 }}>{t(`drepDashboard.overview.details.${key}`)}</Box>
                <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0, mt: 0.75, overflowWrap: "anywhere", whiteSpace: "pre-line" }}>
                  {field(key) || t("drepDashboard.overview.details.notProvided")}
                </Box>
              </Box>
            ))}
          </Box>
          <Panel title={t("drepDashboard.retired.summary")} titleId="retired-summary-title">
            <Box sx={{ display: "flex", flexWrap: "wrap", gap: 1.5 }}>
              {months !== undefined ? (
                <Tile label={t("drepDashboard.retired.tenure")} testId="retired-tenure" value={t("drepDashboard.retired.tenureMonths", { count: months })} />
              ) : (
                <Tile label={t("drepDashboard.retired.registeredOn")} testId="retired-tenure" value={formatLongDate(dRep?.latestRegistrationDate) || "–"} />
              )}
              <Tile label={t("drepDashboard.retired.totalVotes")} testId="retired-total-votes" value={t("drepDashboard.retired.totalVotesValue", { count: cast.length })} />
              <Tile
                label={peak !== undefined ? t("drepDashboard.retired.peak") : t("drepDashboard.retired.final")}
                testId="retired-peak"
                value={powerText(peak ?? finalPower)}
              />
            </Box>
            <Box data-testid="retired-deposit-note" sx={{ alignItems: "center", backgroundColor: ds.success["50"], borderRadius: `${RADIUS.md}px`, color: ds.success["700"], display: "flex", fontSize: 14, fontWeight: 500, gap: 1, px: 2, py: 1.5 }}>
              <CircleCheck aria-hidden size={16} style={{ flexShrink: 0 }} />
              {refunded
                ? t("drepDashboard.retired.deposit", { deposit: refunded })
                : t("drepDashboard.retired.depositNoAmount")}
            </Box>
          </Panel>
          <Panel title={t("drepDashboard.retired.archive")} titleId="retired-archive-title">
            {archived.length ? (
              <Box component="ul" sx={{ display: "flex", flexDirection: "column", gap: 1, listStyle: "none", m: 0, p: 0 }}>
                {archived.map(({ proposal, vote }) => (
                  <Box key={`${proposal.txHash}#${proposal.index}`} component="li" data-testid="retired-archived-vote" sx={{ alignItems: "center", backgroundColor: ds.gray["50"], borderRadius: `${RADIUS.md}px`, display: "flex", gap: 1.5, justifyContent: "space-between", px: 1.5, py: 1.5 }}>
                    <Box sx={{ minWidth: 0 }}>
                      <Box sx={{ color: ds.gray["800"], fontSize: 14, fontWeight: 500 }}>{proposal.title || t("govActions.card.untitled")}</Box>
                      <Box sx={{ color: ds.gray["600"], fontSize: 12 }}>{t("drepDashboard.retired.archived", { date: formatShortDate(vote?.date) })}</Box>
                    </Box>
                    {vote && (
                      <Box component="span" sx={{ backgroundColor: ds.gray["100"], borderRadius: `${RADIUS.pill}px`, color: ds.gray["700"], flexShrink: 0, fontSize: 12, fontWeight: 600, px: 1 }}>
                        {t(`drepDashboard.vote.${vote.vote}`)}
                      </Box>
                    )}
                  </Box>
                ))}
              </Box>
            ) : (
              <Box sx={{ color: ds.gray["600"], fontSize: 14 }}>{t("drepDashboard.retired.noVotes")}</Box>
            )}
          </Panel>
          <Box sx={{ boxShadow: SHADOWS.card, display: "flex", flexDirection: "column", gap: 2.5 }}>{actionCards}</Box>
        </Box>
      </Box>
    </Box>
  );
};
