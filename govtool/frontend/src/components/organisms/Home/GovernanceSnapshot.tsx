import { ReactNode } from "react";
import { Box } from "@mui/material";
import { Check, Circle } from "lucide-react";
import { format, formatDistanceToNow } from "date-fns";

import { useAppContext } from "@context";
import {
  useGetDecidedActionsQuery,
  useGetGovernanceActionMetadata,
  useGetLiveProposalsQuery,
  useTranslation,
} from "@hooks";
import { getProposalTypeLabel } from "@utils";
import { ds, RADIUS } from "@/consts/designTokens";

import { decidedStatus } from "@/utils/governanceActionFormat";

const THIRTY_DAYS = 30 * 24 * 60 * 60 * 1000;

const Metric = ({
  bg,
  dot,
  label,
  value,
  caption,
}: {
  bg: string;
  dot: string;
  label: string;
  value: ReactNode;
  caption: string;
}) => (
  <Box
    sx={{
      backgroundColor: bg,
      borderRadius: `${RADIUS.lg}px`,
      display: "flex",
      flex: 1,
      flexDirection: "column",
      gap: 0.5,
      minWidth: 0,
      p: 2,
    }}
  >
    <Box sx={{ alignItems: "center", display: "flex", gap: 0.75 }}>
      <Box sx={{ backgroundColor: dot, borderRadius: "50%", height: 6, width: 6 }} />
      <Box component="span" sx={{ color: ds.gray["500"], fontSize: 12, fontWeight: 500, lineHeight: "18px" }}>
        {label}
      </Box>
    </Box>
    <Box component="span" sx={{ color: ds.gray["900"], fontSize: 24, fontWeight: 600, letterSpacing: "-0.47px", lineHeight: "36px" }}>
      {value}
    </Box>
    <Box component="span" sx={{ color: ds.gray["500"], fontSize: 12, lineHeight: "18px" }}>
      {caption}
    </Box>
  </Box>
);

const Activity = ({
  heading,
  icon,
  title,
  meta,
  dataTestId,
}: {
  heading: string;
  icon: ReactNode;
  title: string;
  meta: string;
  dataTestId: string;
}) => (
  <Box data-testid={dataTestId}>
    <Box component="p" sx={{ color: ds.gray["500"], fontSize: 14, fontWeight: 500, lineHeight: "20px", m: 0, pb: 1 }}>
      {heading}
    </Box>
    <Box sx={{ alignItems: "center", borderTop: `1px solid ${ds.gray["200"]}`, display: "flex", gap: 1.5, pt: 1.5 }}>
      {icon}
      <Box sx={{ minWidth: 0 }}>
        <Box component="p" sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600, lineHeight: "20px", m: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
          {title}
        </Box>
        <Box component="p" sx={{ color: ds.gray["500"], fontSize: 13, lineHeight: "20px", m: 0 }}>
          {meta}
        </Box>
      </Box>
    </Box>
  </Box>
);

const iconCircle = (bg: string, child: ReactNode) => (
  <Box sx={{ alignItems: "center", backgroundColor: bg, borderRadius: "50%", display: "flex", flexShrink: 0, height: 28, justifyContent: "center", width: 28 }}>
    {child}
  </Box>
);

/** The hero's live card: counts, the latest decision and what is being voted on. */
export const GovernanceSnapshot = () => {
  const { t } = useTranslation();
  const { epochParams } = useAppContext();
  const { liveProposals, liveProposalsTotal } = useGetLiveProposalsQuery();
  const { decidedActions } = useGetDecidedActionsQuery();
  const epoch = epochParams?.epoch_no ?? undefined;

  const typeCount = new Set(liveProposals.map((proposal) => proposal.type)).size;
  const closingThisEpoch = liveProposals.filter(
    (proposal) => epoch !== undefined && proposal.expiryEpochNo === epoch,
  ).length;
  const now = Date.now();
  const recentlyRatified = decidedActions.filter((action) => {
    const decided = decidedStatus(action);
    return (
      decided?.ratified &&
      decided.time &&
      now - new Date(decided.time).getTime() <= THIRTY_DAYS
    );
  }).length;

  const latest = decidedActions[0];
  const latestStatus = latest ? decidedStatus(latest) : undefined;
  const { metadata } = useGetGovernanceActionMetadata(latest);
  const latestTitle =
    latest?.title ??
    metadata?.data?.title ??
    (latest ? getProposalTypeLabel(latest.type) : undefined);

  const votingOn = liveProposals[0];

  return (
    <Box
      data-testid="governance-snapshot"
      sx={{
        backgroundColor: "#FFFFFF",
        border: `1px solid ${ds.gray["200"]}`,
        borderRadius: `${RADIUS.lg}px`,
        boxShadow: "0px 12px 32px 0px rgba(16, 35, 61, 0.06)",
        boxSizing: "border-box",
        display: "flex",
        flexDirection: "column",
        gap: 2.5,
        maxWidth: 510,
        p: 3,
        width: "100%",
      }}
    >
      <Box sx={{ alignItems: "center", display: "flex", justifyContent: "space-between" }}>
        <Box>
          <Box component="h2" sx={{ color: "#172033", fontSize: 16, fontWeight: 600, letterSpacing: "-0.18px", lineHeight: "24px", m: 0 }}>
            {t("landing.snapshot.title")}
          </Box>
          <Box component="p" sx={{ color: ds.gray["500"], fontSize: 12, lineHeight: "18px", m: 0, mt: 0.5 }}>
            {t("landing.snapshot.subtitle")}
          </Box>
        </Box>
        {epoch !== undefined && (
          <Box component="span" sx={{ backgroundColor: ds.primary["100"], borderRadius: `${RADIUS.pill}px`, color: ds.primary["600"], fontSize: 12, fontWeight: 600, lineHeight: "18px", px: 0.75, py: 0.25 }}>
            {t("landing.snapshot.epoch", { epoch })}
          </Box>
        )}
      </Box>
      <Box sx={{ borderTop: `1px solid ${ds.gray["200"]}`, display: "flex", flexDirection: { xxs: "column", sm: "row" }, gap: 1, pt: 2.5 }}>
        <Metric
          bg={ds.primary["50"]}
          caption={t("landing.snapshot.acrossTypes", { count: typeCount })}
          dot={ds.primary["600"]}
          label={t("landing.snapshot.activeActions")}
          value={liveProposalsTotal}
        />
        <Metric
          bg={ds.warning["50"]}
          caption={t("landing.snapshot.ofInVoting", { total: liveProposalsTotal })}
          dot={ds.orange["600"]}
          label={t("landing.snapshot.closingThisEpoch")}
          value={closingThisEpoch}
        />
        <Metric
          bg="#ECFDF3"
          caption={t("landing.snapshot.lastThirtyDays")}
          dot={ds.success["600"]}
          label={t("landing.snapshot.recentlyRatified")}
          value={recentlyRatified}
        />
      </Box>
      <Activity
        dataTestId="snapshot-latest-decision"
        heading={t("landing.snapshot.latestDecision")}
        icon={iconCircle(ds.success["100"], <Check aria-hidden color={ds.success["600"]} size={14} strokeWidth={2.5} />)}
        meta={
          latestStatus?.time
            ? t("landing.snapshot.decidedAgo", {
                status: t(`landing.outcomes.${latestStatus.key}`),
                ago: formatDistanceToNow(new Date(latestStatus.time), { addSuffix: true }),
              })
            : ""
        }
        title={latestTitle ?? t("landing.snapshot.nothingYet")}
      />
      <Activity
        dataTestId="snapshot-voting-on"
        heading={t("landing.snapshot.votingOn")}
        icon={iconCircle(ds.primary["100"], <Circle aria-hidden color={ds.primary["600"]} size={12} strokeWidth={3} />)}
        meta={
          votingOn?.expiryDate
            ? t("landing.snapshot.endsOn", { date: format(new Date(votingOn.expiryDate), "MMM d, yyyy") })
            : ""
        }
        title={
          votingOn
            ? votingOn.title ?? getProposalTypeLabel(votingOn.type)
            : t("landing.snapshot.nothingYet")
        }
      />
    </Box>
  );
};
