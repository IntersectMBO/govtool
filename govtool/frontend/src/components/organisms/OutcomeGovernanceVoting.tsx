import { Box, Divider } from "@mui/material";

import { Typography } from "@atoms";
import { SECURITY_RELEVANT_PARAMS_MAP } from "@consts";
import { useTranslation } from "@hooks";
import type { EpochParams, OutcomeGovernanceAction } from "@models";
import {
  OutcomeIndicator,
  OutcomeStatusChip,
  OutcomeVoteSection,
} from "@molecules";
import { outcomeVoteResult } from "@utils";

/** Display each provider tally independently at the action's own tally epoch. */
export const OutcomeGovernanceVoting = ({
  action,
}: {
  action: OutcomeGovernanceAction;
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
      title: t("outcome.votes.dReps"),
      shortTitle: t("outcome.votes.dReps"),
      prefix: "DReps",
      applicable: true,
    },
    {
      role: "spo",
      title: t("outcome.votes.sPos"),
      shortTitle: t("outcome.votes.sPos"),
      prefix: "SPOs",
      applicable: spoApplicable,
    },
    {
      role: "cc",
      title: t("outcome.votes.cCommitteeFull"),
      shortTitle: t("outcome.votes.cCommitteeShort"),
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
          {t("outcome.ratifiedStatus.title")}
        </Typography>
        <OutcomeStatusChip label={t(`outcome.status.${statusKey}`)} />
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
        {t("outcome.votes.title")}
      </Typography>
      {groups.map((group) => (
        <Box key={group.role}>
          <OutcomeVoteSection
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
          {t("outcome.label")}
        </Typography>
        <Box display="flex" justifyContent="space-between" width="100%" gap={1}>
          {groups.map((group) => {
            const aggregate = action.vote_aggregates?.find(
              (a) => a.role === group.role,
            );
            const result = aggregate && outcomeVoteResult(aggregate);
            return (
              <OutcomeIndicator
                key={group.role}
                title={group.shortTitle}
                passed={type === "InfoAction" ? undefined : result?.passing}
                isDisplayed={group.applicable && type !== "InfoAction"}
                isLoading={false}
                dataTestId={`${group.prefix}-voting-results-outcome`}
              />
            );
          })}
        </Box>
      </Box>
    </Box>
  );
};
