import { useEffect, useState } from "react";
import {
  Box,
  ButtonBase,
  CircularProgress,
  Dialog,
  MenuItem,
  Select,
  SxProps,
} from "@mui/material";
import AssignmentLateOutlinedIcon from "@mui/icons-material/AssignmentLateOutlined";

import { Button, Typography } from "@atoms";
import { useSnackbar } from "@context";
import {
  useGetMetadataReportQuery,
  useGetMetadataReportsQuery,
  useGetMetadataResolveQuery,
  useTranslation,
} from "@hooks";
import {
  METADATA_REPORT_LIST_LIMIT,
  type MetadataAnchor,
  type MetadataFailure,
  type MetadataRefreshOutcome,
} from "@models";
import { openInNewTab } from "@utils";
import { LINKS } from "@/consts/links";
import { errorRed } from "@/consts/colors";

import { MetadataReportView } from "./MetadataReportView";
import { MetadataRetryButton } from "./MetadataRetryButton";
import { REPORT_BORDER } from "./metadataReportStyles";

type MetadataFailureDetailsProps = {
  anchor: MetadataAnchor | null | undefined;
  /**
   * The report of the check that just failed, such as a submission's
   * (D152). Shown instead of looking the anchor up, whose cached answer can
   * differ from what the url served just now.
   */
  reportId?: string;
  /** Why the check could not run at all, when it could not. */
  error?: string;
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
 * The fetch report behind a metadata failure (D113, D117), opened in a dialog
 * from a "View Error Report" button. It renders nothing when the backend
 * reports no failure for the anchor, or when it cannot answer (no metadata
 * service configured): the legacy label stays the source of the user-facing
 * status either way.
 */
export const MetadataFailureDetails = ({
  anchor,
  reportId,
  error,
  canRetry,
  onRecovered,
  sx,
}: MetadataFailureDetailsProps) => {
  const { t } = useTranslation();
  const { addSuccessAlert } = useSnackbar();
  const [isOpen, setIsOpen] = useState(false);
  const [selectedReportId, setSelectedReportId] = useState<string | null>(
    null,
  );
  const [retryNotice, setRetryNotice] = useState<string | null>(null);

  const isOwnCheck = !!reportId || !!error;
  const { metadataResult } = useGetMetadataResolveQuery(
    isOwnCheck ? null : anchor,
  );
  const failure: MetadataFailure | undefined =
    metadataResult && metadataResult.ok === false ? metadataResult : undefined;
  const latestReportId = reportId ?? failure?.reportId ?? null;

  // A retry that fetched again moves the latest report; follow it.
  useEffect(() => {
    setSelectedReportId(null);
  }, [latestReportId]);

  const shownReportId = selectedReportId ?? latestReportId;
  const { reports: listed } = useGetMetadataReportsQuery(
    anchor,
    isOpen && !!latestReportId,
  );
  const reports = listed.slice(0, METADATA_REPORT_LIST_LIMIT);
  const {
    report,
    isLoading: isReportLoading,
    isError: isReportError,
  } = useGetMetadataReportQuery(shownReportId, isOpen);

  const onOutcome = (outcome: MetadataRefreshOutcome) => {
    if (outcome.result.ok) {
      const message = t("metadataReport.retry.success");
      setRetryNotice(message);
      setIsOpen(false);
      addSuccessAlert(message);
      onRecovered?.();
    } else if (outcome.refetched) {
      setRetryNotice(t("metadataReport.retry.stillFailing"));
    }
  };

  if (!anchor) return null;
  if (!failure && !isOwnCheck) {
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
  const isShownListed = reports.some(({ id }) => id === shownReportId);

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
      {hasReport && (
        <ButtonBase
          data-testid="metadata-error-report-button"
          aria-haspopup="dialog"
          onClick={() => setIsOpen(true)}
          sx={{
            alignSelf: "flex-start",
            gap: 0.75,
            px: 1.5,
            py: 0.75,
            borderRadius: 50,
            bgcolor: errorRed.c100,
            color: errorRed.c700,
            fontFamily: "Poppins",
            fontSize: "16px",
            "&:hover": { bgcolor: errorRed.c200 },
          }}
        >
          <AssignmentLateOutlinedIcon fontSize="small" />
          {t("metadataReport.viewReport")}
        </ButtonBase>
      )}
      {!hasReport && canRetry && (
        <MetadataRetryButton anchor={anchor} onOutcome={onOutcome} />
      )}

      {error && (
        <Typography
          variant="body2"
          data-testid="metadata-check-error"
          sx={{ fontFamily: "monospace", wordBreak: "break-word" }}
        >
          {error}
        </Typography>
      )}

      {hasReport && (
        <Dialog
          open={isOpen}
          onClose={() => setIsOpen(false)}
          maxWidth="md"
          fullWidth
          aria-labelledby="metadata-report-title"
          data-testid="metadata-report-dialog"
          PaperProps={{
            sx: {
              borderRadius: "16px",
              bgcolor: "neutralWhite",
              maxWidth: "1000px",
              m: { xxs: 2, md: 4 },
              width: { xxs: "calc(100% - 32px)", md: "calc(100% - 64px)" },
            },
          }}
        >
          <Box
            sx={{
              display: "flex",
              alignItems: "center",
              justifyContent: "space-between",
              flexWrap: "wrap",
              gap: 2,
              px: 3,
              py: 2,
              borderBottom: REPORT_BORDER,
            }}
          >
            <Box
              id="metadata-report-title"
              sx={{ display: "flex", alignItems: "center", gap: 1 }}
            >
              <AssignmentLateOutlinedIcon />
              <Typography sx={{ fontSize: "20px", fontWeight: 600 }}>
                {t("metadataReport.title")}
              </Typography>
            </Box>
            {isShownListed && (
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
                inputProps={{
                  "data-testid": "metadata-report-history",
                  "aria-label": t("metadataReport.history.label"),
                }}
                sx={{
                  fontSize: 14,
                  width: { xxs: "100%", sm: "auto" },
                  maxWidth: "100%",
                  borderRadius: 2,
                }}
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
            )}
          </Box>

          <Box sx={{ overflowY: "auto" }}>
            {isReportLoading && (
              <Box
                sx={{ display: "flex", alignItems: "center", gap: 1, p: 3 }}
              >
                <CircularProgress size={16} />
                <Typography variant="body2">
                  {t("metadataReport.loading")}
                </Typography>
              </Box>
            )}
            {isReportError && (
              <Typography variant="body2" sx={{ color: "errorRed", p: 3 }}>
                {t("metadataReport.loadError")}
              </Typography>
            )}
            {!isReportLoading && !isReportError && report === null && (
              <Typography variant="body2" sx={{ p: 3 }}>
                {t("metadataReport.notFound")}
              </Typography>
            )}
            {report && <MetadataReportView report={report} />}
          </Box>

          <Box
            sx={{
              display: "flex",
              alignItems: "center",
              flexWrap: "wrap",
              gap: 1.5,
              px: 3,
              py: 2,
              bgcolor: "#F3F4F8",
              borderTop: REPORT_BORDER,
            }}
          >
            <Button
              data-testid="metadata-report-documentation"
              size="large"
              variant="outlined"
              onClick={() => openInNewTab(LINKS.DREP_ERROR_CONDITIONS)}
              sx={{
                bgcolor: "neutralWhite",
                borderColor: "#D6D8E0",
                color: "textBlack",
                "&:hover": { bgcolor: "neutralWhite", borderColor: "#B8BBC6" },
              }}
            >
              {t("metadataReport.readDocumentation")}
            </Button>
            {canRetry && (
              <MetadataRetryButton anchor={anchor} onOutcome={onOutcome} />
            )}
            {retryNotice && (
              <Typography variant="body2" data-testid="metadata-retry-notice">
                {retryNotice}
              </Typography>
            )}
          </Box>
        </Dialog>
      )}
    </Box>
  );
};
