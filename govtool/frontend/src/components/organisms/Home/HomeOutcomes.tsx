import { ReactNode } from "react";
import { Box, IconButton } from "@mui/material";
import { useQueries } from "@tanstack/react-query";
import { useNavigate } from "react-router";
import { BadgeCheck, Flag, MoreVertical, UsersRound } from "lucide-react";

import { Button } from "@atoms";
import { GOV_ACTION_HISTORY_PATHS, QUERY_KEYS } from "@consts";
import {
  useGetDecidedActionsQuery,
  useGetGovernanceActionMetadata,
  useTranslation,
} from "@hooks";
import type {
  GovernanceActionRecord,
  GovernanceActionVoteAggregate,
} from "@models";
import { getGovernanceActionRecord } from "@services";
import {
  correctAdaFormatWithSuffix,
  getProposalTypeLabel,
  toGovernanceActionId,
} from "@utils";
import { ds, FONT_FAMILY, PILL_TONES, RADIUS } from "@/consts/designTokens";

import { decidedStatus } from "@/utils/governanceActionFormat";
import { DisplayHeading, Eyebrow, HomeColumn, Lead } from "./homeParts";

const THIRTY_DAYS = 30 * 24 * 60 * 60 * 1000;
const SHOWN = 3;

const Pill = ({
  tone,
  children,
}: {
  tone: keyof typeof PILL_TONES;
  children: ReactNode;
}) => (
  <Box
    component="span"
    sx={{
      backgroundColor: PILL_TONES[tone].bg,
      borderRadius: `${RADIUS.xs}px`,
      color: tone === "success" ? ds.success["600"] : PILL_TONES[tone].text,
      fontSize: 12,
      fontWeight: 500,
      lineHeight: "18px",
      px: 0.75,
    }}
  >
    {children}
  </Box>
);

const Stat = ({
  icon,
  value,
  label,
}: {
  icon: ReactNode;
  value: string;
  label: string;
}) => (
  <Box
    sx={{
      alignItems: "center",
      backgroundColor: "#FFFFFF",
      border: `1px solid ${ds.gray["200"]}`,
      borderRadius: `${RADIUS.lg}px`,
      display: "flex",
      gap: 1.5,
      p: 2,
    }}
  >
    <Box sx={{ alignItems: "center", backgroundColor: ds.success["50"], borderRadius: `${RADIUS.md}px`, color: ds.success["600"], display: "flex", flexShrink: 0, height: 32, justifyContent: "center", width: 32 }}>
      {icon}
    </Box>
    <Box>
      <Box component="p" sx={{ color: ds.gray["900"], fontSize: 20, fontWeight: 600, letterSpacing: "-0.33px", lineHeight: "30px", m: 0 }}>
        {value}
      </Box>
      <Box component="p" sx={{ color: ds.gray["500"], fontSize: 13, lineHeight: "20px", m: 0 }}>
        {label}
      </Box>
    </Box>
  </Box>
);

/** "₳ 3.67B - 90%" for stake, "19 - 65%" for counts: the yes amount and share. */
const tallyText = (aggregate: GovernanceActionVoteAggregate) => {
  const total = Number(aggregate.totalEligible) - Number(aggregate.abstain);
  const yes = Number(aggregate.yes);
  const share = total > 0 ? `${Math.round((yes / total) * 100)}%` : "—";
  const amount =
    aggregate.representation === "stake"
      ? `₳ ${correctAdaFormatWithSuffix(yes, 2)}`
      : aggregate.yes;
  return `${amount} - ${share}`;
};

const OutcomeCard = ({
  action,
  record,
}: {
  action: GovernanceActionRecord;
  record?: GovernanceActionRecord;
}) => {
  const { t } = useTranslation();
  const navigate = useNavigate();
  const decided = decidedStatus(action);
  const { metadata } = useGetGovernanceActionMetadata(record ?? action);
  const title =
    record?.title ?? metadata?.data?.title ?? getProposalTypeLabel(action.type);
  const abstract = record?.abstract ?? metadata?.data?.abstract;
  const ratified = decided?.ratified ?? false;
  const detailPath = `${GOV_ACTION_HISTORY_PATHS.governanceActionHistory}/${action.id}`;

  return (
    <Box data-testid="outcome-row" sx={{ display: "grid", gap: 2, gridTemplateColumns: "24px 1fr" }}>
      <Box sx={{ alignItems: "center", display: "flex", flexDirection: "column" }}>
        <Box
          aria-hidden
          sx={{
            border: `2px solid ${ratified ? ds.success["600"] : ds.error["600"]}`,
            borderRadius: "50%",
            boxSizing: "border-box",
            height: 16,
            mt: 3,
            position: "relative",
            width: 16,
            "&::after": {
              backgroundColor: ratified ? ds.success["600"] : ds.error["600"],
              borderRadius: "50%",
              content: '""',
              height: 6,
              left: 3,
              position: "absolute",
              top: 3,
              width: 6,
            },
          }}
        />
        <Box sx={{ backgroundColor: ds.gray["200"], flex: 1, mt: 1, width: "1px" }} />
      </Box>
      <Box
        sx={{
          backgroundColor: "#FFFFFF",
          border: `1px solid ${ds.gray["200"]}`,
          borderRadius: `${RADIUS.lg}px`,
          boxShadow: "0px 1px 2px 0px rgba(16, 24, 40, 0.04)",
          mb: 2,
          minWidth: 0,
          p: 2.5,
        }}
      >
        <Box sx={{ alignItems: "flex-start", display: "flex", gap: 1, justifyContent: "space-between" }}>
          <Box sx={{ display: "flex", flexWrap: "wrap", gap: 0.75 }}>
            {decided?.key === "enacted" && <Pill tone="success">{t("landing.outcomes.ratified")}</Pill>}
            {decided && (
              <Pill tone={ratified ? "success" : "error"}>
                {t(`landing.outcomes.${decided.key}`)}
              </Pill>
            )}
            {!ratified && <Pill tone="neutral">{t("landing.outcomes.notRatified")}</Pill>}
            <Pill tone="neutral">{getProposalTypeLabel(action.type)}</Pill>
          </Box>
          <IconButton
            aria-label={title}
            onClick={() => navigate(detailPath)}
            size="small"
            sx={{ border: `1px solid ${ds.gray["200"]}`, height: 32, width: 32 }}
          >
            <MoreVertical aria-hidden color={ds.gray["700"]} size={16} />
          </IconButton>
        </Box>
        <Box component="h3" sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600, letterSpacing: "-0.18px", lineHeight: "24px", m: 0, mt: 1 }}>
          {title}
        </Box>
        {abstract && (
          <Lead sx={{ display: "-webkit-box", mt: 0.5, overflow: "hidden", WebkitBoxOrient: "vertical", WebkitLineClamp: 2, whiteSpace: "normal" }}>
            {abstract}
          </Lead>
        )}
        {!!record?.vote_aggregates?.length && (
          <Box sx={{ color: ds.gray["500"], display: "flex", flexWrap: "wrap", fontFamily: FONT_FAMILY.mono, fontSize: 12, gap: 2, lineHeight: "18px", mt: 1.5 }}>
            {record.vote_aggregates.map((aggregate) => (
              <Box key={aggregate.role} component="span">
                {`${aggregate.role === "drep" ? "DRep" : aggregate.role.toUpperCase()} `}
                <Box component="span" sx={{ color: ds.gray["900"] }}>
                  {tallyText(aggregate)}
                </Box>
              </Box>
            ))}
          </Box>
        )}
      </Box>
    </Box>
  );
};

/** Recent decisions: three summary figures and the latest three outcomes. */
export const HomeOutcomes = () => {
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { decidedActions } = useGetDecidedActionsQuery();
  const shown = decidedActions.slice(0, SHOWN);
  const records = useQueries({
    queries: shown.map((action) => {
      const actionId = toGovernanceActionId(`${action.tx_hash}#${action.index}`);
      return {
        queryKey: [QUERY_KEYS.useGetGovernanceActionRecordKey, actionId],
        queryFn: () => getGovernanceActionRecord(actionId),
      };
    }),
  });

  const now = Date.now();
  const recent = decidedActions
    .map((action) => decidedStatus(action))
    .filter(
      (decided) =>
        decided?.time && now - new Date(decided.time).getTime() <= THIRTY_DAYS,
    );
  const ratifiedShare = recent.length
    ? `${Math.round((recent.filter((decided) => decided?.ratified).length / recent.length) * 100)}%`
    : "—";
  // DRep participation on the outcomes shown: stake that voted over stake eligible.
  const participations = records
    .map((query) => query.data?.vote_aggregates?.find((a) => a.role === "drep"))
    .filter((a): a is GovernanceActionVoteAggregate => !!a && Number(a.totalEligible) > 0)
    .map((a) => 1 - Number(a.notVoted) / Number(a.totalEligible));
  const participation = participations.length
    ? `${Math.round((participations.reduce((sum, value) => sum + value, 0) / participations.length) * 100)}%`
    : "—";

  return (
    <Box component="section" sx={{ py: { xxs: 6, md: 10 } }}>
      <HomeColumn>
        <Box sx={{ alignItems: { md: "flex-end" }, display: "flex", flexDirection: { xxs: "column", md: "row" }, gap: 2, justifyContent: "space-between" }}>
          <Box sx={{ maxWidth: 520 }}>
            <Eyebrow>{t("landing.outcomes.eyebrow")}</Eyebrow>
            <DisplayHeading sx={{ mt: 2 }}>{t("landing.outcomes.title")}</DisplayHeading>
            <Lead sx={{ maxWidth: 340, mt: 2 }}>{t("landing.outcomes.description")}</Lead>
          </Box>
          <Button
            onClick={() => navigate(GOV_ACTION_HISTORY_PATHS.governanceActionHistory)}
            sx={{ color: ds.gray["900"] }}
            variant="outlined"
          >
            {t("landing.outcomes.viewOutcomes")}
          </Button>
        </Box>
        <Box sx={{ display: "grid", gap: 1.5, gridTemplateColumns: { xxs: "1fr", md: "repeat(3, 1fr)" }, mt: 4 }}>
          <Stat icon={<Flag aria-hidden size={16} />} label={t("landing.outcomes.decidedThirtyDays")} value={String(recent.length)} />
          <Stat icon={<BadgeCheck aria-hidden size={16} />} label={t("landing.outcomes.ratificationRate")} value={ratifiedShare} />
          <Stat icon={<UsersRound aria-hidden size={16} />} label={t("landing.outcomes.votingParticipation")} value={participation} />
        </Box>
        <Box sx={{ mt: 3 }}>
          {shown.map((action, index) => (
            <OutcomeCard key={action.id} action={action} record={records[index]?.data} />
          ))}
        </Box>
      </HomeColumn>
    </Box>
  );
};
