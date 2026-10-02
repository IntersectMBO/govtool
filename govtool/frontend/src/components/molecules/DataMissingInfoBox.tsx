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

export const DataMissingInfoBox = ({
  isDataMissing,
  isInProgress,
  isValidating,
  isSubmitted,
  isDrep = false,
  issues,
  sx,
}: {
  isDataMissing?: MetadataValidationStatus;
  isInProgress?: boolean;
  isValidating?: boolean;
  isSubmitted?: boolean;
  isDrep?: boolean;
  /** The rules the document breaks, listed under the description. */
  issues?: MetadataIssue[];
  sx?: SxProps;
}) => {
  const { t } = useTranslation();

  const errorKey = isDataMissing && getMetadataStatusErrorKey(isDataMissing);
  const scope = isDrep ? "errors.dRep" : "errors.gAMetadata";
  const gaMetadataErrorMessage = errorKey && t(`${scope}.message.${errorKey}`);
  const gaMetadataErrorDescription =
    errorKey && t(`${scope}.description.${errorKey}`);
  const errors = getMetadataErrors(issues);

  return isDataMissing && !isSubmitted && !isInProgress ? (
    <Box
      sx={{
        mb: 4,
        pr: 6,
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
          }}
        >
          {gaMetadataErrorMessage}
        </Typography>
      )}
      {isValidating ? (
        <Skeleton
          sx={{ mb: 0.5 }}
          width="100%"
          height="96px"
          variant="rounded"
        />
      ) : (
        <Typography
          data-testid="metadata-error-description"
          sx={{
            fontWeight: 400,
            color: "errorRed",
            mb: 0.5,
          }}
        >
          {gaMetadataErrorDescription}
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
        <Skeleton width="128px" height="24px" variant="text" />
      ) : (
        <Link
          data-testid="metadata-error-learn-more"
          onClick={() => openInNewTab(LINKS.DREP_ERROR_CONDITIONS)}
          sx={{
            fontFamily: "Poppins",
            fontSize: "16px",
            lineHeight: "24px",
            cursor: "pointer",
          }}
        >
          {t("learnMore")}
        </Link>
      )}
    </Box>
  ) : null;
};
