import { useState } from "react";
import {
  Box,
  Fade,
  Grid,
  LinearProgress,
  Skeleton,
  styled,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
} from "@mui/material";
import {
  IconCheveronDown,
  IconCheveronUp,
} from "@intersect.mbo/intersectmbo.org-icons-set";

import { Typography } from "@atoms";
import { primaryBlue, successGreen } from "@consts";
import { useTranslation } from "@hooks";
import { formatOutcomeVoteValue, lovelaceToRoundedUpAda } from "@utils";

const THRESHOLD_COLOR = "#45458A";
const NO_BAR_COLOR = "#D84444";

type VoteMetric = {
  label: string;
  value: number | string;
  testId?: string;
  isHighlighted?: boolean;
  indentDepth?: number;
};

const ProgressContainer = styled(Box)({
  position: "relative",
  width: "100%",
  height: 32,
  borderRadius: 20,
  overflow: "hidden",
});

const StyledLinearProgress = styled(LinearProgress)({
  height: "100%",
  borderRadius: 10,
  backgroundColor: NO_BAR_COLOR,
  ".MuiLinearProgress-bar": {
    backgroundColor: successGreen.c500,
  },
});

const PercentageOverlay = styled(Box)({
  position: "absolute",
  top: 0,
  width: "100%",
  height: "100%",
  display: "flex",
  alignItems: "center",
  justifyContent: "space-between",
  pointerEvents: "none",
});

const PercentageText = styled(Box)(({ theme }) => ({
  fontSize: 13,
  fontWeight: 600,
  color: theme.palette.textBlack,
  padding: "0 10px",
  zIndex: 10,
  whiteSpace: "nowrap",
  "@media (max-width: 599.95px)": {
    padding: "0 4px",
  },
}));

const ThresholdIndicator = styled(Box, {
  shouldForwardProp: (prop) => prop !== "left",
})<{ left: number }>(({ left }) => ({
  position: "absolute",
  left: `${left}%`,
  top: "-24px",
  transform: "translateX(-50%)",
  display: "flex",
  flexDirection: "column",
  alignItems: "center",
  zIndex: 5,
}));

const VoteSectionLoader = ({ title }: { title: string }) => (
  <Box mb={3}>
    <Typography
      sx={{ color: "neutralGray", fontWeight: 600, fontSize: 18, mb: 1 }}
    >
      {title}
    </Typography>
    <Skeleton
      variant="rectangular"
      width="100%"
      height={32}
      sx={{ borderRadius: 20, mb: 1 }}
    />
    <Skeleton
      variant="rectangular"
      width="100%"
      height={105}
      sx={{ borderRadius: 1 }}
    />
  </Box>
);

type VoteMetricsTableProps = {
  collapsedMetrics: VoteMetric[];
  expandedMetrics: VoteMetric[];
  title: string;
  isCC: boolean;
};

const VoteMetricsTable = ({
  collapsedMetrics,
  expandedMetrics,
  title,
  isCC,
}: VoteMetricsTableProps) => {
  const { t } = useTranslation();
  const [expanded, setExpanded] = useState(false);
  const [transitioning, setTransitioning] = useState(false);

  const toggleExpand = () => {
    setTransitioning(true);
    setTimeout(() => {
      setExpanded((prev) => !prev);
      setTimeout(() => setTransitioning(false), 50);
    }, 150);
  };

  const metrics = expanded ? expandedMetrics : collapsedMetrics;

  return (
    <Box>
      <Fade in={!transitioning} timeout={300}>
        <Table
          sx={{
            boxShadow: "0px 10px 10px -5px rgba(33, 42, 61, 0.08)",
            border: "1px solid #F1F1F4",
          }}
        >
          <TableHead>
            <TableRow sx={{ backgroundColor: "#FCFCFC", py: 1, px: 2 }}>
              <TableCell
                sx={{
                  p: "inherit",
                  textAlign: "start",
                  fontWeight: 500,
                  fontSize: 14,
                }}
              >
                {t("outcome.votes.metric")}
              </TableCell>
              <TableCell
                sx={{
                  p: "inherit",
                  textAlign: "end",
                  fontWeight: 500,
                  fontSize: 14,
                }}
              >
                {t("outcome.votes.value")} {!isCC && " (₳)"}
              </TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {metrics.map((metric) => (
              <TableRow
                key={metric.label}
                sx={{
                  backgroundColor: metric.isHighlighted ? "#F9F9FB" : "inherit",
                  py: metric.isHighlighted ? 1 : 2,
                  px: 2,
                }}
              >
                <TableCell sx={{ p: "inherit", textAlign: "start" }}>
                  <Typography
                    sx={{
                      fontWeight: metric.isHighlighted ? 600 : 400,
                      fontSize: 14,
                      lineHeight: 1.75,
                      marginLeft: `${(metric.indentDepth ?? 0) * 8}px`,
                    }}
                  >
                    {metric.label}
                  </Typography>
                </TableCell>
                <TableCell sx={{ p: "inherit", textAlign: "end" }}>
                  <Box
                    component="span"
                    data-testid={metric.testId}
                    sx={{ color: "textBlack" }}
                  >
                    {isCC
                      ? metric.value
                      : lovelaceToRoundedUpAda(
                          Number(metric.value),
                        ).toLocaleString()}
                  </Box>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </Fade>

      {!isCC && (
        <Box
          onClick={toggleExpand}
          data-testid={`${title}-${expanded ? "collapse" : "expand"}-button`}
          sx={{
            alignItems: "center",
            color: "primaryBlue",
            cursor: "pointer",
            display: "flex",
            fontSize: 14,
            fontWeight: 600,
            gap: 1,
            justifyContent: "center",
            mt: 2,
            py: 1,
          }}
        >
          {expanded ? t("outcome.collapse") : t("outcome.expand")}
          {expanded ? (
            <IconCheveronUp fill={primaryBlue.c500} />
          ) : (
            <IconCheveronDown fill={primaryBlue.c500} />
          )}
        </Box>
      )}
    </Box>
  );
};

type OutcomeVoteSectionProps = {
  title: string;
  yesVotes?: number;
  noVotes?: number;
  noTotalVotes?: number;
  noConfidenceVotes?: number;
  totalControlled?: number;
  totalAbstainVotes?: number;
  autoAbstainVotes?: number;
  explicitAbstainVotes?: number;
  notVotedVotes?: number;
  threshold?: number | null;
  ratificationThreshold?: number;
  yesPercentage?: number;
  noPercentage?: number;
  isCC?: boolean;
  isDisplayed: boolean;
  isDataReady: boolean;
  dataTestId?: string;
};

/**
 * One voter group's result: the yes/no bar against the ratification
 * threshold, and the stake (or, for the committee, member) breakdown.
 */
export const OutcomeVoteSection = ({
  title,
  yesVotes = 0,
  noVotes = 0,
  noTotalVotes = 0,
  totalControlled = 0,
  totalAbstainVotes = 0,
  autoAbstainVotes = 0,
  explicitAbstainVotes = 0,
  noConfidenceVotes = 0,
  notVotedVotes = 0,
  threshold = null,
  yesPercentage = 0,
  noPercentage = 0,
  ratificationThreshold = 0,
  isCC = false,
  isDisplayed,
  isDataReady,
  dataTestId,
}: OutcomeVoteSectionProps) => {
  const { t } = useTranslation();

  if (!isDataReady) {
    return <VoteSectionLoader title={title} />;
  }

  const collapsedMetrics: VoteMetric[] = isCC
    ? [
        {
          label: t("outcome.votes.numberOfCCs"),
          value: totalControlled,
          testId: "active-constitutional-committee-count",
        },
        {
          label: t("outcome.votes.abstainVotes"),
          value: totalAbstainVotes,
          testId: "constitutional-committee-abstain-votes",
        },
        {
          label: t("outcome.votes.notVoted"),
          value: notVotedVotes,
          testId: "constitutional-committee-not-voted-votes",
        },
      ]
    : [
        {
          label: t("outcome.votes.totalActiveStake"),
          value: totalControlled,
          testId: `${title}-total-controlled-amount`,
        },
        {
          label: t("outcome.votes.totalAbstain"),
          value: totalAbstainVotes,
          testId: `${title}-abstain-votes`,
        },
        {
          label: t("outcome.votes.ratificationThreshold"),
          value: ratificationThreshold,
          testId: `${title}-ratification-threshold`,
        },
      ];

  const expandedMetrics: VoteMetric[] = [
    {
      label: t("outcome.votes.totalActiveStake"),
      value: totalControlled,
      testId: `${title}-total-controlled-amount`,
    },
    {
      label: t("outcome.votes.ratificationThreshold"),
      value: ratificationThreshold,
      testId: `${title}-ratification-threshold`,
      isHighlighted: true,
      indentDepth: 1,
    },
    {
      label: t("outcome.votes.yes"),
      value: yesVotes,
      testId: `${title}-yes-votes`,
      indentDepth: 2,
    },
    {
      label: t("outcome.votes.no"),
      value: noVotes,
      testId: `${title}-no-votes`,
      indentDepth: 2,
    },
    {
      label: t("outcome.votes.noConfidence"),
      value: noConfidenceVotes,
      testId: `${title}-no-confidence-votes`,
      indentDepth: 2,
    },
    {
      label: t("outcome.votes.notVoted"),
      value: notVotedVotes,
      testId: `${title}-not-voted-votes`,
      indentDepth: 2,
    },
    {
      label: t("outcome.votes.totalAbstain"),
      value: totalAbstainVotes,
      testId: `${title}-abstain-votes`,
      isHighlighted: true,
      indentDepth: 1,
    },
    {
      label: t("outcome.votes.autoAbstain"),
      value: autoAbstainVotes,
      testId: `${title}-auto-abstain`,
      indentDepth: 2,
    },
    {
      label: t("outcome.votes.explicit"),
      value: explicitAbstainVotes,
      testId: `${title}-explicit-abstain`,
      indentDepth: 2,
    },
  ];

  const thresholdValue = threshold
    ? isCC
      ? Math.round((totalControlled - totalAbstainVotes) * threshold)
      : threshold * ratificationThreshold
    : 0;

  return (
    <Box
      data-testid={dataTestId}
      mb={3}
      // Plain Box text (bar labels, totals, expand) otherwise falls back to
      // the browser's serif; only Typography gets the theme font.
      sx={{ fontFamily: "Poppins, Arial" }}
    >
      <Typography
        data-testid={`${title}-outcome-voter-label`}
        sx={{ fontWeight: 600, fontSize: 16, mb: 1.875 }}
      >
        {title}
      </Typography>
      {!isDisplayed ? (
        <Typography
          data-testid="voting-not-available-label"
          sx={{ fontWeight: 400, fontSize: 13 }}
        >
          {title}{" "}
          <span>
            <strong>{t("outcome.votes.votingNotAvailable")}</strong>{" "}
            {t("outcome.votes.onThisTypeOfAction")}
          </span>
        </Typography>
      ) : (
        <Grid container spacing={1.875}>
          <Grid item xs={12}>
            <Box position="relative" width="100%">
              {threshold !== null && (
                <>
                  <ThresholdIndicator left={threshold * 100}>
                    <Box
                      sx={{
                        backgroundColor: THRESHOLD_COLOR,
                        border: `1px solid ${THRESHOLD_COLOR}`,
                        borderRadius: "12px",
                        boxShadow: "0px 1px 2px rgba(0,0,0,0.08)",
                        padding: "2px 8px",
                      }}
                    >
                      <Typography
                        data-testid={`${title}-outcome-threshold`}
                        sx={{
                          color: "white",
                          fontSize: 12,
                          fontWeight: 600,
                          lineHeight: 1.2,
                          whiteSpace: "nowrap",
                        }}
                      >
                        {formatOutcomeVoteValue(thresholdValue, isCC)} -{" "}
                        {(threshold * 100).toFixed(0)}%
                      </Typography>
                    </Box>
                    <Box
                      sx={{
                        borderLeft: "6px solid transparent",
                        borderRight: "6px solid transparent",
                        borderTop: `6px solid ${THRESHOLD_COLOR}`,
                        height: 0,
                        marginTop: "-1px",
                        width: 0,
                      }}
                    />
                  </ThresholdIndicator>
                  <Box
                    sx={{
                      backgroundColor: THRESHOLD_COLOR,
                      height: "100%",
                      left: `${threshold * 100}%`,
                      position: "absolute",
                      transform: "translateX(-50%)",
                      width: "2px",
                      zIndex: 4,
                    }}
                  />
                </>
              )}
              <ProgressContainer>
                <StyledLinearProgress
                  data-testid={`${title}-percentages-progress-bar`}
                  variant="determinate"
                  value={yesPercentage}
                />
                <PercentageOverlay>
                  <PercentageText>{t("outcome.votes.yes")}</PercentageText>
                  <PercentageText>{t("outcome.votes.no")}</PercentageText>
                </PercentageOverlay>
              </ProgressContainer>
            </Box>
            <Box
              sx={{
                alignItems: "center",
                display: "flex",
                fontSize: 14,
                fontWeight: 600,
                justifyContent: "space-between",
                lineHeight: 1.75,
                mt: 1,
                width: "100%",
              }}
            >
              <Box
                data-testid={`${title}-yes-votes-submitted`}
                component="span"
              >
                {`${formatOutcomeVoteValue(yesVotes, isCC)} - ${yesPercentage?.toFixed(2)}%`}
              </Box>
              <Box data-testid={`${title}-no-votes-submitted`} component="span">
                {`${formatOutcomeVoteValue(
                  isCC ? noVotes : noTotalVotes,
                  isCC,
                )} - ${noPercentage?.toFixed(2)}%`}
              </Box>
            </Box>
          </Grid>
          <Grid item xs={12}>
            <VoteMetricsTable
              collapsedMetrics={collapsedMetrics}
              expandedMetrics={expandedMetrics}
              title={title}
              isCC={isCC}
            />
          </Grid>
        </Grid>
      )}
    </Box>
  );
};
