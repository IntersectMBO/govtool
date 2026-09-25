import { useEffect, useState } from "react";
import {
  Box,
  CircularProgress,
  Collapse,
  MenuItem,
  Select,
  SxProps,
} from "@mui/material";

import { Button, Typography } from "@atoms";
import { useSnackbar } from "@context";
import {
  useGetMetadataReportQuery,
  useGetMetadataReportsQuery,
  useGetMetadataResolveQuery,
  useTranslation,
} from "@hooks";
import type {
  MetadataAnchor,
  MetadataFailure,
  MetadataRefreshOutcome,
} from "@models";

import { MetadataReportView } from "./MetadataReportView";
import { MetadataRetryButton } from "./MetadataRetryButton";

type MetadataFailureDetailsProps = {
  anchor: MetadataAnchor | null | undefined;
  /** Show the retry button (D118): gov-action pages, and a DRep's own page. */
  canRetry?: boolean;
  /** Called once a retry resolves the document, so the page can reload it. */
  onRecovered?: () => void;
  sx?: SxProps;
};

const formatTimestamp = (value: string) => {
  const date = new Date(value);
  return Number.isNaN(date.getTime()) ? value : date.toLocaleString();
};

/**
 * The fetch report behind a metadata failure (D113, D117), collapsed under the
 * existing error label. It renders nothing when the backend reports no failure
 * for the anchor, or when it cannot answer (no metadata service configured):
 * the legacy label stays the source of the user-facing status either way.
 */
export const MetadataFailureDetails = ({
  anchor,
  canRetry,
  onRecovered,
  sx,
}: MetadataFailureDetailsProps) => {
  const { t } = useTranslation();
  const { addSuccessAlert } = useSnackbar();
  const [isExpanded, setIsExpanded] = useState(false);
  const [selectedReportId, setSelectedReportId] = useState<string | null>(
    null,
  );
  const [retryNotice, setRetryNotice] = useState<string | null>(null);

  const { metadataResult } = useGetMetadataResolveQuery(anchor);
  const failure: MetadataFailure | undefined =
    metadataResult && metadataResult.ok === false ? metadataResult : undefined;
  const latestReportId = failure?.reportId ?? null;

  // A retry that fetched again moves the latest report; follow it.
  useEffect(() => {
    setSelectedReportId(null);
  }, [latestReportId]);

  const shownReportId = selectedReportId ?? latestReportId;
  const { reports } = useGetMetadataReportsQuery(
    anchor,
    isExpanded && !!failure,
  );
  const {
    report,
    isLoading: isReportLoading,
    isError: isReportError,
  } = useGetMetadataReportQuery(shownReportId, isExpanded);

  const onOutcome = (outcome: MetadataRefreshOutcome) => {
    if (outcome.result.ok) {
      const message = t("metadataReport.retry.success");
      setRetryNotice(message);
      addSuccessAlert(message);
      onRecovered?.();
    } else if (outcome.refetched) {
      setRetryNotice(t("metadataReport.retry.stillFailing"));
    }
  };

  if (!anchor) return null;
  if (!failure) {
    return retryNotice ? (
      <Typography
        variant="body2"
        sx={{ color: "positiveGreen", ...sx }}
        data-testid="metadata-retry-success"
      >
        {retryNotice}
      </Typography>
    ) : null;
  }

  const hasReport = !!latestReportId;

  return (
    <Box
      data-testid="metadata-failure-details"
      sx={{
        display: "flex",
        flexDirection: "column",
        gap: 1.5,
        minWidth: 0,
        maxWidth: "100%",
        ...sx,
      }}
    >
      <Box
        sx={{ display: "flex", alignItems: "center", gap: 2, flexWrap: "wrap" }}
      >
        {hasReport && (
          <Button
            data-testid="metadata-failure-details-toggle"
            size="small"
            variant="text"
            aria-expanded={isExpanded}
            onClick={() => setIsExpanded((value) => !value)}
            sx={{ px: 0 }}
          >
            {isExpanded
              ? t("metadataReport.hideDetails")
              : t("metadataReport.showDetails")}
          </Button>
        )}
        {canRetry && (
          <MetadataRetryButton anchor={anchor} onOutcome={onOutcome} />
        )}
      </Box>

      {retryNotice && (
        <Typography variant="body2" data-testid="metadata-retry-notice">
          {retryNotice}
        </Typography>
      )}

      {hasReport && (
        <Collapse in={isExpanded} unmountOnExit>
          <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
            {reports.length > 1 && (
              <Box sx={{ display: "flex", alignItems: "center", gap: 1 }}>
                <Typography variant="body2" color="neutralGray">
                  {t("metadataReport.history.label")}
                </Typography>
                <Select
                  size="small"
                  value={shownReportId ?? ""}
                  onChange={(event) =>
                    setSelectedReportId(
                      event.target.value === latestReportId
                        ? null
                        : (event.target.value as string),
                    )
                  }
                  inputProps={{ "data-testid": "metadata-report-history" }}
                  sx={{ fontSize: 14, maxWidth: "100%" }}
                >
                  {reports.map((summary, index) => (
                    <MenuItem key={summary.id} value={summary.id}>
                      {`${formatTimestamp(summary.startedAt)} · ${
                        summary.code
                      }${
                        index === 0
                          ? ` (${t("metadataReport.history.latest")})`
                          : ""
                      }`}
                    </MenuItem>
                  ))}
                </Select>
              </Box>
            )}

            {isReportLoading && (
              <Box sx={{ display: "flex", alignItems: "center", gap: 1 }}>
                <CircularProgress size={16} />
                <Typography variant="body2">
                  {t("metadataReport.loading")}
                </Typography>
              </Box>
            )}
            {isReportError && (
              <Typography variant="body2" sx={{ color: "errorRed" }}>
                {t("metadataReport.loadError")}
              </Typography>
            )}
            {!isReportLoading && !isReportError && report === null && (
              <Typography variant="body2">
                {t("metadataReport.notFound")}
              </Typography>
            )}
            {report && <MetadataReportView report={report} />}
          </Box>
        </Collapse>
      )}
    </Box>
  );
};
