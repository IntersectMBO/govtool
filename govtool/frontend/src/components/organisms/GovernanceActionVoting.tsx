import { Box, Divider } from "@mui/material";

import { Typography } from "@atoms";
import { SECURITY_RELEVANT_PARAMS_MAP } from "@consts";
import { useTranslation } from "@hooks";
import type { EpochParams, GovernanceActionRecord } from "@models";
import {
  GovernanceActionIndicator,
  GovernanceActionStatusChip,
  GovernanceActionVoteSection,
} from "@molecules";
import { voteAggregateResult } from "@utils";

/** Display each provider tally independently at the action's own tally epoch. */
export const GovernanceActionVoting = ({
  action,
}: {
  action: GovernanceActionRecord;
}) => {
  const { t } = useTranslation();
  const { status, type } = action;
  const securityChange = Object.values(SECURITY_RELEVANT_PARAMS_MAP).some(
    (key) => action.proposal_params?.[key as keyof EpochParams] != null,
  );
  const spoApplicable =
    action.vote_aggregates?.some((aggregate) => aggregate.role === "spo") ||
    [
      "NoConfidence",
      "NewCommittee",
      "HardForkInitiation",
      "InfoAction",
    ].includes(type) ||
    (type === "ParameterChange" && securityChange);
  const ccApplicable = !["NoConfidence", "NewCommittee"].includes(type);
  const groups = [
    {
      role: "drep",
      title: t("actionRecord.votes.dReps"),
      shortTitle: t("actionRecord.votes.dReps"),
      prefix: "DReps",
      applicable: true,
    },
    {
      role: "spo",
      title: t("actionRecord.votes.sPos"),
      shortTitle: t("actionRecord.votes.sPos"),
      prefix: "SPOs",
      applicable: spoApplicable,
    },
    {
      role: "cc",
      title: t("actionRecord.votes.cCommitteeFull"),
      shortTitle: t("actionRecord.votes.cCommitteeShort"),
      prefix: "CC",
      applicable: ccApplicable,
    },
  ];
  const statusKey =
    status.enacted_epoch !== null
      ? "enacted"
      : status.ratified_epoch !== null
        ? "ratified"
        : status.expired_epoch !== null
          ? "expired"
          : status.dropped_epoch !== null
            ? "dropped"
            : "inProgress";

  return (
    <Box>
      <Box
        display="flex"
        justifyContent="space-between"
        alignItems="center"
        width="100%"
        mb={3}
      >
        <Typography sx={{ fontWeight: 500, fontSize: 14, color: "#506288" }}>
          {t("actionRecord.ratifiedStatus.title")}
        </Typography>
        <GovernanceActionStatusChip label={t(`actionRecord.status.${statusKey}`)} />
      </Box>
      <Typography
        sx={{
          fontSize: 22,
          fontWeight: 600,
          lineHeight: "24px",
          wordBreak: "break-word",
          mb: 3,
        }}
      >
        {t("actionRecord.votes.title")}
      </Typography>
      {groups.map((group) => (
        <Box key={group.role}>
          <GovernanceActionVoteSection
            title={group.title}
            aggregate={action.vote_aggregates?.find(
              (a) => a.role === group.role,
            )}
            isDisplayed={group.applicable}
            dataTestId={`${group.prefix}-voting-results-data`}
          />
          <Divider sx={{ my: 2 }} />
        </Box>
      ))}
      <Box mb={1}>
        <Typography sx={{ fontWeight: 600, fontSize: 18, mb: 1.875 }}>
          {t("actionRecord.label")}
        </Typography>
        <Box display="flex" justifyContent="space-between" width="100%" gap={1}>
          {groups.map((group) => {
            const aggregate = action.vote_aggregates?.find(
              (a) => a.role === group.role,
            );
            const result = aggregate && voteAggregateResult(aggregate);
            return (
              <GovernanceActionIndicator
                key={group.role}
                title={group.shortTitle}
                passed={type === "InfoAction" ? undefined : result?.passing}
                isDisplayed={group.applicable && type !== "InfoAction"}
                isLoading={false}
                dataTestId={`${group.prefix}-voting-results-indicator`}
              />
            );
          })}
        </Box>
      </Box>
    </Box>
  );
};
