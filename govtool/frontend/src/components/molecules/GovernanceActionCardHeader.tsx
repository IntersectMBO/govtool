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

type GovernanceActionCardHeaderProps = {
  title?: string;
  isDataMissing?: MetadataValidationStatus;
  /** Errors explain a failure in the tooltip; warnings alone get an icon. */
  metadataIssues?: MetadataIssue[];
  isValidating?: boolean;
  dataTestId?: string;
};

export const GovernanceActionCardHeader = ({
  title,
  isDataMissing,
  metadataIssues,
  isValidating,
  dataTestId = "governance-action-card-header",
}: GovernanceActionCardHeaderProps) => {
  const { t } = useTranslation();
  const errors = getMetadataErrors(metadataIssues);
  const warnings = getMetadataWarnings(metadataIssues);

  return (
    <Box
      sx={{
        display: "flex",
        alignItems: "center",
        mb: "20px",
        overflow: "hidden",
      }}
      data-testid={dataTestId}
    >
      {isValidating ? (
        <Skeleton height="24px" width="100px" variant="rounded" />
      ) : (
        <Typography
          sx={{
            fontSize: 18,
            fontWeight: 600,
            lineHeight: "24px",
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
              color: "#ADAEAD",
            }}
            sx={{ ml: 0.7 }}
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
            sx={{ ml: 0.7, color: "orangeDark" }}
            fontSize="small"
          />
        </Tooltip>
      )}
    </Box>
  );
};
