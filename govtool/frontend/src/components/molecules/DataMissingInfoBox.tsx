import { ReactNode } from "react";
import { Box, Link, Skeleton, SxProps } from "@mui/material";

import { Typography } from "@atoms";
import { useTranslation } from "@hooks";
import { MetadataIssue, MetadataValidationStatus } from "@models";
import {
  getMetadataErrors,
  getMetadataIssueMessage,
  getMetadataStatusErrorKey,
  openInNewTab,
} from "@utils";
import { LINKS } from "@/consts/links";
import { errorRed } from "@/consts/colors";

export const DataMissingInfoBox = ({
  isDataMissing,
  isInProgress,
  isValidating,
  isSubmitted,
  isDrep = false,
  issues,
  reportAction,
  sx,
}: {
  isDataMissing?: MetadataValidationStatus;
  isInProgress?: boolean;
  isValidating?: boolean;
  isSubmitted?: boolean;
  isDrep?: boolean;
  /** The rules the document breaks, listed under the message. */
  issues?: MetadataIssue[];
  /** Shown at the end of the docs link's row, such as the report button. */
  reportAction?: ReactNode;
  sx?: SxProps;
}) => {
  const { t } = useTranslation();

  const errorKey = isDataMissing && getMetadataStatusErrorKey(isDataMissing);
  const scope = isDrep ? "errors.dRep" : "errors.gAMetadata";
  const gaMetadataErrorMessage = errorKey && t(`${scope}.message.${errorKey}`);
  const errors = getMetadataErrors(issues);

  return isDataMissing && !isSubmitted && !isInProgress ? (
    <Box
      sx={{
        mb: 4,
        maxWidth: {
          xxs: "295px",
          md: "100%",
        },
        ...sx,
      }}
    >
      {isValidating ? (
        <Skeleton
          sx={{ mb: 0.5 }}
          width="128px"
          height="48px"
          variant="rounded"
        />
      ) : (
        <Typography
          data-testid="metadata-error-message"
          sx={{
            fontSize: "18px",
            fontWeight: 500,
            color: "errorRed",
            mb: 0.5,
            pr: 6,
          }}
        >
          {gaMetadataErrorMessage}
        </Typography>
      )}
      {!isValidating && errors.length > 0 && (
        <Box
          component="ul"
          data-testid="metadata-error-issues"
          sx={{ color: "errorRed", mt: 0, mb: 0.5, pl: 3 }}
        >
          {errors.map((issue) => (
            <Typography
              component="li"
              key={`${issue.field}-${issue.rule}`}
              sx={{ color: "errorRed", fontWeight: 400 }}
            >
              {getMetadataIssueMessage(issue)}
            </Typography>
          ))}
        </Box>
      )}
      {isValidating ? (
        <Skeleton
          sx={{ mt: 2 }}
          width="100%"
          height="56px"
          variant="rounded"
        />
      ) : (
        <Box
          data-testid="metadata-error-explanation"
          sx={{
            mt: 2,
            py: 1.5,
            display: "flex",
            alignItems: "center",
            justifyContent: "space-between",
            flexWrap: "wrap",
            gap: 1.5,
            borderTop: `1px dashed ${errorRed.c200}`,
            bgcolor: "#FFF7F6",
          }}
        >
          {/* The explanation of each error lives in the docs. */}
          <Link
            component="button"
            type="button"
            data-testid="metadata-error-learn-more"
            onClick={() => openInNewTab(LINKS.DREP_ERROR_CONDITIONS)}
            underline="hover"
            sx={{
              fontFamily: "Poppins",
              fontSize: "16px",
              fontWeight: 600,
              lineHeight: "24px",
              textAlign: "left",
              color: errorRed.c700,
            }}
          >
            {t("metadataReport.whyError")}
          </Link>
          {reportAction}
        </Box>
      )}
    </Box>
  ) : null;
};
