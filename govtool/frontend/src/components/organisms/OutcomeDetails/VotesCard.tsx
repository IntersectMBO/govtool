import { ReactNode, useState } from "react";
import { useTranslation } from "react-i18next";
import { Box, ButtonBase, Dialog, IconButton } from "@mui/material";
import { Server, ShieldCheck, UserRound, X } from "lucide-react";

import { ds, FONT_FAMILY, RADIUS } from "@/consts/designTokens";
import type { GovernanceActionRecord, GovernanceActionVoteAggregate } from "@/models/governanceActions";
import { formatVoteAggregateValue, voteAggregateResult } from "@/utils/voteAggregate";
import { outcomeKey, roleSummary, voterGroups, type VoterRole } from "@/utils/outcomeDetails";

import { CardTitle, Card, compactValue, Divider, VerdictBadge } from "./parts";

// The ids the outcome E2E spec reads: the section by prefix, values by title.
const ROLE_IDS: Record<VoterRole, { prefix: string; title: string }> = {
  drep: { prefix: "DReps", title: "DReps" },
  spo: { prefix: "SPOs", title: "SPOs" },
  cc: { prefix: "CC", title: "Constitutional Committee" },
};

const ROLE_ICONS: Record<VoterRole, ReactNode> = {
  drep: <UserRound aria-hidden color={ds.orange["600"]} size={15} />,
  spo: <Server aria-hidden color={ds.orange["600"]} size={15} />,
  cc: <ShieldCheck aria-hidden color={ds.orange["600"]} size={15} />,
};

const mono = { fontFamily: FONT_FAMILY.mono, fontSize: 11, letterSpacing: "0.3px" };

const Legend = () => {
  const { t } = useTranslation();
  return (
    <Box sx={{ alignItems: "center", color: ds.gray["700"], display: "flex", gap: 1.25, ...mono, textTransform: "uppercase" }}>
      <Box sx={{ alignItems: "center", display: "flex", gap: 0.5 }}>
        <Box aria-hidden sx={{ backgroundColor: ds.success["600"], borderRadius: "2px", height: 8, width: 8 }} />
        {t("outcome.votes.yes")}
      </Box>
      <Box sx={{ alignItems: "center", display: "flex", gap: 0.5 }}>
        <Box aria-hidden sx={{ backgroundColor: ds.error["600"], borderRadius: "2px", height: 8, width: 8 }} />
        {t("outcome.votes.no")}
      </Box>
      <Box aria-hidden sx={{ borderLeft: `1px solid ${ds.gray["300"]}`, height: 10 }} />
      {t("outcome.votes.threshold")}
    </Box>
  );
};

/** Yes in green, the rest of the counted stake in red, the threshold marked. */
type TallyBarProps = {
  aggregate: GovernanceActionVoteAggregate;
  role: VoterRole;
  /** The card's bar carries the ids; the dialog's copy does not. */
  withIds?: boolean;
};

const TallyBar = ({ aggregate, role, withIds = false }: TallyBarProps) => {
  const summary = roleSummary(aggregate)!;
  const { title } = ROLE_IDS[role];
  return (
    <Box>
      <Box
        aria-label={`${title}: ${summary.yesPercent}% yes, threshold ${summary.thresholdPercent}%`}
        aria-valuemax={100}
        aria-valuemin={0}
        aria-valuenow={summary.yesPercent}
        role={withIds ? "progressbar" : undefined}
        sx={{ backgroundColor: ds.error["400"], borderRadius: "4px", height: 8, mt: 3, position: "relative" }}
      >
        <Box sx={{ backgroundColor: ds.success["500"], borderRadius: "4px 0 0 4px", height: "100%", width: `${summary.yesPercent}%` }} />
        <Box aria-hidden sx={{ left: `${summary.thresholdPercent}%`, position: "absolute", top: -22, transform: "translateX(-50%)" }}>
          <Box sx={{ backgroundColor: ds.gray["900"], borderRadius: `${RADIUS.pill}px`, color: "#FFFFFF", fontSize: 11, fontWeight: 600, px: 0.75, py: 0.125 }}>
            {`${Math.round(summary.thresholdPercent)}%`}
          </Box>
          <Box sx={{ backgroundColor: ds.gray["900"], height: 14, mx: "auto", width: "2px" }} />
        </Box>
      </Box>
      <Box sx={{ display: "flex", fontSize: 12, justifyContent: "space-between", mt: 1 }}>
        <Box data-testid={withIds ? `${title}-yes-votes-submitted` : undefined} sx={{ color: ds.gray["900"], fontWeight: 600 }}>
          {compactValue(summary.yes, aggregate.representation)}
          <Box component="span" sx={{ color: ds.success["700"], fontWeight: 400 }}>{` • ${summary.yesPercent.toFixed(2)}%`}</Box>
        </Box>
        <Box sx={{ color: ds.gray["900"], fontWeight: 600 }}>
          {compactValue(summary.rest, aggregate.representation)}
          <Box component="span" sx={{ color: ds.error["700"], fontWeight: 400 }}>{` • ${summary.restPercent.toFixed(2)}%`}</Box>
        </Box>
      </Box>
    </Box>
  );
};

/** A count tally is members, not stake. */
const figureLabels = (representation?: GovernanceActionVoteAggregate["representation"]) => (
  representation === "count"
    ? { total: "outcome.votes.totalActiveMembers", threshold: "outcome.votes.thresholdMembers" }
    : { total: "outcome.votes.totalActive", threshold: "outcome.votes.thresholdStake" });

const MetricRows = ({ rows }: { rows: [string, string, string?][] }) => (
  <Box component="dl" sx={{ display: "flex", flexDirection: "column", gap: 1, m: 0 }}>
    {rows.map(([label, value, testId]) => (
      <Box key={label} sx={{ alignItems: "baseline", display: "flex", fontSize: 13, justifyContent: "space-between" }}>
        <Box component="dt" sx={{ color: ds.gray["600"] }}>{label}</Box>
        <Box component="dd" data-testid={testId} sx={{ color: ds.gray["900"], fontWeight: 500, m: 0 }}>{value}</Box>
      </Box>
    ))}
  </Box>
);

/** Every figure of one role's tally, in a dialog. */
const AllDataDialog = ({
  role,
  aggregate,
  onClose,
}: {
  role: VoterRole;
  aggregate: GovernanceActionVoteAggregate;
  onClose: () => void;
}) => {
  const { t } = useTranslation();
  const summary = roleSummary(aggregate);
  const { title } = ROLE_IDS[role];
  const value = (raw: string) => formatVoteAggregateValue(raw, aggregate.representation);
  const labels = figureLabels(aggregate.representation);
  const rows: [string, string, string?][] = [
    [t(labels.total), value(aggregate.totalEligible), `${title}-total-votes`],
    [t(labels.threshold), summary ? value(summary.thresholdValue) : "—"],
    [t("outcome.votes.yes"), value(aggregate.yes), `${title}-yes-votes`],
    [t("outcome.votes.no"), value(aggregate.no), `${title}-no-votes`],
    [t("outcome.votes.notVoted"), value(aggregate.notVoted), `${title}-not-voted-votes`],
    [t("outcome.votes.totalAbstain"), value(aggregate.abstain), `${title}-abstain-votes`],
  ];
  return (
    <Dialog
      aria-labelledby={`outcome-all-data-title-${role}`}
      onClose={onClose}
      open
      PaperProps={{ sx: { borderRadius: `${RADIUS.xl}px`, borderTop: `3px solid ${ds.primary["300"]}`, maxWidth: 640, p: { xxs: 2, md: 3 }, width: "100%" } }}
    >
      <Box sx={{ alignItems: "center", display: "flex", justifyContent: "space-between", mb: 2 }}>
        <Box component="h2" id={`outcome-all-data-title-${role}`} sx={{ color: ds.gray["900"], fontSize: 18, fontWeight: 600, m: 0 }}>
          {t("outcome.votes.title")}
        </Box>
        <IconButton aria-label={t("outcome.votes.close")} data-testid="outcome-all-data-close" onClick={onClose} size="small">
          <X size={18} />
        </IconButton>
      </Box>
      <Box sx={{ backgroundColor: ds.gray["50"], border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, px: 2.5, py: 2 }}>
        <Box sx={{ alignItems: "center", color: ds.gray["900"], display: "flex", fontSize: 14, fontWeight: 600, gap: 0.75 }}>
          {ROLE_ICONS[role]}
          {ROLE_IDS[role].title}
        </Box>
        {summary && <TallyBar aggregate={aggregate} role={role} />}
        <Box sx={{ mt: 2 }}><Legend /></Box>
      </Box>
      <Box component="table" sx={{ border: `1px solid ${ds.gray["200"]}`, borderCollapse: "collapse", borderRadius: `${RADIUS.md}px`, fontSize: 14, mt: 2, width: "100%" }}>
        <Box component="thead">
          <Box component="tr" sx={{ backgroundColor: ds.gray["50"] }}>
            <Box component="th" sx={{ color: ds.gray["900"], fontWeight: 600, px: 2, py: 1.25, textAlign: "left" }}>{t("outcome.votes.metric")}</Box>
            <Box component="th" sx={{ color: ds.gray["900"], fontWeight: 600, px: 2, py: 1.25, textAlign: "right" }}>{t("outcome.votes.value")}</Box>
          </Box>
        </Box>
        <Box component="tbody">
          {rows.map(([label, figure, testId]) => (
            <Box key={label} component="tr" sx={{ borderTop: `1px solid ${ds.gray["200"]}` }}>
              <Box component="td" sx={{ color: ds.gray["600"], px: 2, py: 1.25 }}>{label}</Box>
              <Box component="td" data-testid={testId} sx={{ color: ds.gray["900"], px: 2, py: 1.25, textAlign: "right" }}>{figure}</Box>
            </Box>
          ))}
        </Box>
      </Box>
    </Dialog>
  );
};

/**
 * One role's block: its bar and figures, or why it has none (does not vote
 * on this type, no tally served, a tally that does not add up, or nothing
 * left once abstentions are taken out). See all data opens in place.
 */
const RoleBlock = ({
  role,
  applicable,
  aggregate,
}: {
  role: VoterRole;
  applicable: boolean;
  aggregate?: GovernanceActionVoteAggregate;
}) => {
  const { t } = useTranslation();
  const [isOpen, setIsOpen] = useState(false);
  const { prefix, title } = ROLE_IDS[role];
  const result = aggregate ? voteAggregateResult(aggregate) : undefined;
  const summary = aggregate ? roleSummary(aggregate) : undefined;
  const name = role === "cc" ? title : prefix;
  const labels = figureLabels(aggregate?.representation);
  const value = (raw: string) => formatVoteAggregateValue(raw, aggregate!.representation);

  let status: string | undefined;
  if (!applicable) status = t(role === "spo" ? "outcome.votes.spoNotVoting" : "outcome.votes.ccNotVoting");
  else if (!aggregate) status = t("actionRecord.votes.dataUnavailable");
  else if (!result) status = t("actionRecord.votes.dataInconsistent");

  return (
    <Box data-testid={`${prefix}-voting-results-data`} sx={{ display: "flex", flexDirection: "column", gap: 1.5 }}>
      <Box sx={{ alignItems: "center", color: ds.gray["900"], display: "flex", fontSize: 14, fontWeight: 600, gap: 0.75 }}>
        {ROLE_ICONS[role]}
        {name}
      </Box>
      {status ? (
        <>
          <Box aria-hidden sx={{ backgroundImage: `repeating-linear-gradient(135deg, ${ds.gray["200"]} 0 2px, transparent 2px 5px)`, borderRadius: "4px", height: 8 }} />
          <Box role="status" sx={{ color: ds.gray["600"], fontSize: 13 }}>{status}</Box>
        </>
      ) : (
        <>
          {summary ? (
            <TallyBar aggregate={aggregate!} role={role} withIds />
          ) : (
            <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{t("actionRecord.votes.noEligibleVotes")}</Box>
          )}
          {role === "cc" && aggregate!.representation === "count" ? (
            <MetricRows
              rows={[
                [t("outcome.votes.constitutional"), Number(aggregate!.yes).toLocaleString("en-US")],
                [t("outcome.votes.unconstitutional"), Number(aggregate!.no).toLocaleString("en-US")],
                [t("outcome.votes.notVoted"), Number(aggregate!.notVoted).toLocaleString("en-US")],
              ]}
            />
          ) : (
            <MetricRows
              rows={[
                [t(labels.total), value(aggregate!.totalEligible)],
                [t("outcome.votes.totalAbstain"), value(aggregate!.abstain)],
                [t(labels.threshold), summary ? value(summary.thresholdValue) : "—"],
              ]}
            />
          )}
          <ButtonBase
            data-testid={`${title}-expand-button`}
            onClick={() => setIsOpen(true)}
            sx={{ backgroundColor: ds.gray["50"], border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.pill}px`, color: ds.gray["900"], fontFamily: "inherit", fontSize: 13, fontWeight: 600, height: 36, "&:hover": { backgroundColor: ds.gray["100"] }, "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` } }}
          >
            {t("outcome.votes.seeAll")}
          </ButtonBase>
          {isOpen && (
            <AllDataDialog aggregate={aggregate!} onClose={() => setIsOpen(false)} role={role} />
          )}
        </>
      )}
    </Box>
  );
};

const OUTCOME_COLORS = {
  enacted: ds.success["700"],
  ratified: ds.success["700"],
  expired: ds.error["700"],
  dropped: ds.error["700"],
  live: ds.primary["600"],
};

/**
 * The right-hand card: each role's tally with its threshold, the overall
 * verdict per role, and how the action ended.
 */
export const VotesCard = ({ action }: { action: GovernanceActionRecord }) => {
  const { t } = useTranslation();
  const groups = voterGroups(action);
  const outcome = outcomeKey(action);

  return (
    <Card testId="single-action-voting-numbers" sx={{ display: "flex", flexDirection: "column", gap: 2.5 }}>
      <Box>
        <CardTitle>{t("outcome.votes.title")}</CardTitle>
        <Box sx={{ mt: 1.5 }}><Legend /></Box>
      </Box>
      {groups.map((group, index) => (
        <Box key={group.role} sx={{ display: "flex", flexDirection: "column", gap: 2.5 }}>
          {index > 0 && <Divider dashed />}
          <RoleBlock aggregate={group.aggregate} applicable={group.applicable} role={group.role} />
        </Box>
      ))}
      <Divider dashed />
      <Box sx={{ display: "flex", flexDirection: "column", gap: 1.25 }}>
        <Box>
          <CardTitle as="h3">{t("outcome.votes.overall")}</CardTitle>
          <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{t("outcome.votes.overallBody")}</Box>
        </Box>
        {groups.map((group) => (
          <VerdictBadge
            key={group.role}
            aggregate={group.aggregate}
            applicable={group.applicable}
            isInfo={action.type === "InfoAction"}
            role={group.role}
            size="row"
          />
        ))}
        <Box
          data-testid="outcome-result"
          role="status"
          sx={{ backgroundColor: OUTCOME_COLORS[outcome], borderRadius: `${RADIUS.md}px`, color: "#FFFFFF", fontSize: 14, fontWeight: 600, py: 1.25, textAlign: "center" }}
        >
          {t("outcome.votes.outcome", { status: t(`outcome.statuses.${outcome}`) })}
        </Box>
      </Box>
    </Card>
  );
};
