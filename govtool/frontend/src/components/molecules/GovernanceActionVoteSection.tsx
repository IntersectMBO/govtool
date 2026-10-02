import { useState } from "react";
import {
  Box,
  Fade,
  Grid,
  LinearProgress,
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
import { formatVoteAggregateValue, voteAggregateResult } from "@utils";
import type { GovernanceActionVoteAggregate } from "@models";

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

type VoteMetricsTableProps = {
  collapsedMetrics: VoteMetric[];
  expandedMetrics: VoteMetric[];
  title: string;
  representation: GovernanceActionVoteAggregate["representation"];
};

const VoteMetricsTable = ({
  collapsedMetrics,
  expandedMetrics,
  title,
  representation,
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
                {t("actionRecord.votes.metric")}
              </TableCell>
              <TableCell
                sx={{
                  p: "inherit",
                  textAlign: "end",
                  fontWeight: 500,
                  fontSize: 14,
                }}
              >
                {t("actionRecord.votes.value")}
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
                    {formatVoteAggregateValue(
                      String(metric.value),
                      representation,
                    )}
                  </Box>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </Fade>

      {expandedMetrics.length > collapsedMetrics.length && (
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
          {expanded ? t("actionRecord.collapse") : t("actionRecord.expand")}
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

type GovernanceActionVoteSectionProps = {
  title: string;
  aggregate?: GovernanceActionVoteAggregate;
  isDisplayed: boolean;
  dataTestId: string;
};

/** A complete action-specific tally, with no network-wide recomputation. */
export const GovernanceActionVoteSection = ({
  title,
  aggregate,
  isDisplayed,
  dataTestId,
}: GovernanceActionVoteSectionProps) => {
  const { t } = useTranslation();
  const result = aggregate && voteAggregateResult(aggregate);
  const unavailable = !aggregate ? "dataUnavailable" : "dataInconsistent";

  if (!isDisplayed || !aggregate || !result) {
    return (
      <Box data-testid={dataTestId} mb={3}>
        <Typography sx={{ fontWeight: 600, fontSize: 16, mb: 1.875 }}>
          {title}
        </Typography>
        <Box
          role="status"
          data-testid={
            isDisplayed
              ? `${dataTestId}-unavailable`
              : "voting-not-available-label"
          }
        >
          <Typography>
            {isDisplayed
              ? t(`actionRecord.votes.${unavailable}`)
              : `${title} ${t("actionRecord.votes.votingNotAvailable")} ${t("actionRecord.votes.onThisTypeOfAction")}`}
          </Typography>
        </Box>
      </Box>
    );
  }

  const { representation, yes, no, abstain, notVoted, totalEligible } =
    aggregate;
  const { yesPercentage, noPercentage } = result;
  const threshold =
    aggregate.threshold.numerator / aggregate.threshold.denominator;
  const collapsedMetrics: VoteMetric[] = [
    {
      label: t(
        representation === "count"
          ? "actionRecord.votes.eligibleMembers"
          : "actionRecord.votes.totalEligible",
      ),
      value: totalEligible,
      testId:
        representation === "count"
          ? "active-constitutional-committee-count"
          : `${title}-total-controlled-amount`,
    },
    {
      label: t("actionRecord.votes.totalAbstain"),
      value: abstain,
      testId: `${title}-abstain-votes`,
    },
    {
      label: t("actionRecord.votes.ratificationDenominator"),
      value: result.ratification,
      testId: `${title}-ratification-threshold`,
    },
  ];
  const expandedMetrics: VoteMetric[] = [
    ...collapsedMetrics,
    { label: t("actionRecord.votes.yes"), value: yes, testId: `${title}-yes-votes` },
    { label: t("actionRecord.votes.no"), value: no, testId: `${title}-no-votes` },
    {
      label: t("actionRecord.votes.notVoted"),
      value: notVoted,
      testId: `${title}-not-voted-votes`,
    },
  ];

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
      <Grid container spacing={1.875}>
        <Grid item xs={12}>
          {yesPercentage === undefined ? (
            <Typography>{t("actionRecord.votes.noEligibleVotes")}</Typography>
          ) : (
            <>
              <Box position="relative" width="100%">
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
                        {(threshold * 100).toFixed(2)}%
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
                <ProgressContainer>
                  <StyledLinearProgress
                    data-testid={`${title}-percentages-progress-bar`}
                    variant="determinate"
                    value={yesPercentage}
                  />
                  <PercentageOverlay>
                    <PercentageText>{t("actionRecord.votes.yes")}</PercentageText>
                    <PercentageText>{t("actionRecord.votes.no")}</PercentageText>
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
                  {`${formatVoteAggregateValue(yes, representation)} - ${yesPercentage.toFixed(2)}%`}
                </Box>
                <Box
                  data-testid={`${title}-no-votes-submitted`}
                  component="span"
                >
                  {`${formatVoteAggregateValue(result.totalNo, representation)} - ${noPercentage?.toFixed(2)}%`}
                </Box>
              </Box>
            </>
          )}
        </Grid>
        <Grid item xs={12}>
          <VoteMetricsTable
            collapsedMetrics={collapsedMetrics}
            expandedMetrics={expandedMetrics}
            title={title}
            representation={representation}
          />
        </Grid>
      </Grid>
    </Box>
  );
};
