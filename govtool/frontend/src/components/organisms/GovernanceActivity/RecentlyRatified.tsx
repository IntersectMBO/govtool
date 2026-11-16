import { Box } from "@mui/material";
import { useQueries } from "@tanstack/react-query";

import { GOV_ACTION_HISTORY_PATHS, QUERY_KEYS } from "@consts";
import {
  useGetDecidedActionsQuery,
  useGetGovernanceActionMetadata,
  useTranslation,
} from "@hooks";
import type { GovernanceActionRecord } from "@models";
import { getGovernanceActionRecord } from "@services";
import {
  getGovernanceActionCIP129Id,
  getProposalTypeLabel,
  toGovernanceActionId,
} from "@utils";
import { ds } from "@/consts/designTokens";
import { voteBar } from "@/utils/governanceActionFormat";
import { formatUtc } from "@/utils/governanceActivity";

import {
  ActivityBand,
  ActivityCard,
  Badge,
  Category,
  LabelledValue,
  SectionHeading,
  TextLink,
} from "./activityParts";

const SHOWN = 3;

/** "gov_action1…8f2c", short enough for a table row. */
export const shortActionId = (id: string) =>
  (id.length > 16 ? `${id.slice(0, 11)}…${id.slice(-4)}` : id);

/** "DReps 72% Yes · SPOs 61% Yes" from the roles that voted on the action. */
const outcomeText = (
  record: GovernanceActionRecord | undefined,
  yes: (role: string, value: string) => string,
) => {
  const parts = (record?.vote_aggregates ?? [])
    .map((aggregate) => {
      const bar = voteBar(aggregate);
      return bar && Number(aggregate.totalEligible) > 0
        ? yes(aggregate.role, `${Math.round(bar.yesPercent)}%`)
        : undefined;
    })
    .filter(Boolean);
  return parts.length ? parts.join(" · ") : "—";
};

const RatifiedRow = ({
  action,
  record,
}: {
  action: GovernanceActionRecord;
  record?: GovernanceActionRecord;
}) => {
  const { t } = useTranslation();
  const { metadata } = useGetGovernanceActionMetadata(record ?? action);
  const typeLabel = getProposalTypeLabel(action.type);
  const title = record?.title ?? action.title ?? metadata?.data?.title ?? typeLabel;

  return (
    <Box
      component="li"
      data-testid="ratified-row"
      sx={{
        alignItems: { md: "center" },
        borderTop: `1px solid ${ds.gray["200"]}`,
        display: "grid",
        gap: { xxs: 1.5, md: 2.5 },
        gridTemplateColumns: { xxs: "1fr 1fr", md: "minmax(0, 1fr) 150px 260px 110px" },
        py: 2.375,
        "&:first-of-type": { borderTop: "none" },
      }}
    >
      <Box sx={{ display: "flex", flexDirection: "column", gap: 0.875, gridColumn: { xxs: "1 / -1", md: "auto" }, minWidth: 0 }}>
        <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1 }}>
          <Badge tone="success">{t("activity.ratified.label")}</Badge>
          {action.status.enacted_epoch !== null && (
            <Badge tone="success">{t("activity.ratified.enacted")}</Badge>
          )}
          <Category>{typeLabel}</Category>
        </Box>
        <Box component="h3" sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600, letterSpacing: "-0.18px", lineHeight: "24px", m: 0, overflowWrap: "anywhere" }}>
          {title}
        </Box>
        <Box component="span" sx={{ color: ds.gray["500"], fontSize: 12, lineHeight: "18px" }}>
          {shortActionId(getGovernanceActionCIP129Id(action))}
        </Box>
      </Box>
      <LabelledValue label={t("activity.ratified.label")}>
        <Box component="time" dateTime={action.status_times.ratified_time ?? undefined} sx={{ color: ds.success["700"], fontSize: 13, fontWeight: 600, lineHeight: "19.5px" }}>
          {formatUtc(action.status_times.ratified_time, "date")}
        </Box>
      </LabelledValue>
      <LabelledValue label={t("activity.ratified.outcome")}>
        <Box component="span" sx={{ color: ds.gray["900"], fontSize: 13, fontWeight: 600, lineHeight: "19.5px" }}>
          {outcomeText(record, (role, value) =>
            t("activity.ratified.yes", { role: t(`activity.ratified.roles.${role}`), value }))}
        </Box>
      </LabelledValue>
      <Box sx={{ display: "flex", gridColumn: { xxs: "1 / -1", md: "auto" }, justifyContent: { md: "flex-end" } }}>
        <TextLink
          label={`${t("activity.ratified.viewAction")}: ${title}`}
          to={`${GOV_ACTION_HISTORY_PATHS.governanceActionHistory}/${action.id}`}
        >
          {t("activity.ratified.viewAction")}
        </TextLink>
      </Box>
    </Box>
  );
};

/** The latest ratifications with each role's yes share. Hidden when there are none. */
export const RecentlyRatified = () => {
  const { t } = useTranslation();
  const { decidedActions } = useGetDecidedActionsQuery();
  const shown = decidedActions
    .filter((action) => !!action.status_times.ratified_time)
    .sort(
      (a, b) =>
        new Date(b.status_times.ratified_time ?? 0).getTime() -
        new Date(a.status_times.ratified_time ?? 0).getTime(),
    )
    .slice(0, SHOWN);
  // The same keys as Home's outcomes, so the records are shared.
  const records = useQueries({
    queries: shown.map((action) => {
      const actionId = toGovernanceActionId(`${action.tx_hash}#${action.index}`);
      return {
        queryKey: [QUERY_KEYS.useGetGovernanceActionRecordKey, actionId],
        queryFn: () => getGovernanceActionRecord(actionId),
      };
    }),
  });

  if (!shown.length) return null;

  return (
    <ActivityBand labelledBy="recently-ratified-heading" sx={{ display: "flex", flexDirection: "column", gap: 3 }}>
      <SectionHeading
        description={t("activity.ratified.description")}
        id="recently-ratified-heading"
        link={
          <TextLink to={`${GOV_ACTION_HISTORY_PATHS.governanceActionHistory}?status=ratified`}>
            {t("activity.ratified.viewAll")}
          </TextLink>
        }
        title={t("activity.ratified.title")}
      />
      <ActivityCard dataTestId="recently-ratified">
        <Box component="ul" sx={{ listStyle: "none", m: 0, p: 0 }}>
          {shown.map((action, index) => (
            <RatifiedRow key={action.id} action={action} record={records[index]?.data} />
          ))}
        </Box>
      </ActivityCard>
    </ActivityBand>
  );
};
