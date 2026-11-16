import { Box, Skeleton } from "@mui/material";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";
import WarningAmberRoundedIcon from "@mui/icons-material/WarningAmberRounded";

import { Tooltip, Typography } from "@atoms";
import { useTranslation } from "@hooks";
import {
  getMetadataDataMissingStatusTranslation,
  getMetadataErrors,
  getMetadataIssueMessage,
  getMetadataWarnings,
} from "@/utils";
import { MetadataIssue, MetadataValidationStatus } from "@/models";

import { ds } from "@/consts/designTokens";

type GovernanceActionCardHeaderProps = {
  title?: string;
  isDataMissing?: MetadataValidationStatus;
  /** Errors explain a failure in the tooltip; warnings alone get an icon. */
  metadataIssues?: MetadataIssue[];
  isValidating?: boolean;
  dataTestId?: string;
  /** The list row's smaller title, as a heading, with no bottom margin. */
  compact?: boolean;
};

export const GovernanceActionCardHeader = ({
  title,
  isDataMissing,
  metadataIssues,
  isValidating,
  dataTestId = "governance-action-card-header",
  compact = false,
}: GovernanceActionCardHeaderProps) => {
  const { t } = useTranslation();
  const errors = getMetadataErrors(metadataIssues);
  const warnings = getMetadataWarnings(metadataIssues);

  return (
    <Box
      sx={{
        display: "flex",
        alignItems: "center",
        mb: compact ? 0 : "20px",
        overflow: "hidden",
      }}
      data-testid={dataTestId}
    >
      {isValidating ? (
        <Skeleton height="24px" width="100px" variant="rounded" />
      ) : (
        <Typography
          component={compact ? "h3" : "p"}
          sx={{
            fontSize: compact ? 16 : 18,
            fontWeight: 600,
            letterSpacing: compact ? "-0.18px" : undefined,
            lineHeight: "24px",
            m: 0,
            display: "-webkit-box",
            WebkitBoxOrient: "vertical",
            WebkitLineClamp: 2,
            wordBreak: "break-word",
            ...(isDataMissing && { color: "errorRed" }),
          }}
        >
          {(isDataMissing &&
            getMetadataDataMissingStatusTranslation(
              isDataMissing as MetadataValidationStatus,
            )) ||
            title}
        </Typography>
      )}
      {isDataMissing && typeof isDataMissing === "string" && (
        <Tooltip
          heading={getMetadataDataMissingStatusTranslation(
            isDataMissing as MetadataValidationStatus,
          )}
          paragraphOne={
            errors.length > 0
              ? errors.map(getMetadataIssueMessage).join(" ")
              : t("govActions.dataMissingTooltipExplanation")
          }
          paragraphTwo={
            errors.length > 0
              ? t("govActions.dataMissingTooltipExplanation")
              : undefined
          }
          placement="bottom-end"
          arrow
        >
          <InfoOutlinedIcon
            style={{
              color: ds.gray["400"],
            }}
            // Above a list card's stretched link, so the tooltip opens.
            sx={{ ml: 0.7, position: "relative", zIndex: 1 }}
            fontSize="small"
          />
        </Tooltip>
      )}
      {!isDataMissing && !isValidating && warnings.length > 0 && (
        <Tooltip
          heading={t("metadataIssues.warningTitle")}
          paragraphOne={warnings.map(getMetadataIssueMessage).join(" ")}
          placement="bottom-end"
          arrow
        >
          <WarningAmberRoundedIcon
            data-testid="metadata-warning-icon"
            sx={{ ml: 0.7, color: "orangeDark", position: "relative", zIndex: 1 }}
            fontSize="small"
          />
        </Tooltip>
      )}
    </Box>
  );
};
