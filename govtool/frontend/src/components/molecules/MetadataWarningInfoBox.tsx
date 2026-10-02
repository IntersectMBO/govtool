import { Box, SxProps } from "@mui/material";

import { Typography } from "@atoms";
import { useTranslation } from "@hooks";
import { MetadataIssue } from "@models";
import { getMetadataIssueMessage, getMetadataWarnings } from "@utils";

/**
 * Shown above valid content whose document still breaks a non-blocking rule
 * of its standard, such as a CIP-108 title longer than 80 characters.
 */
export const MetadataWarningInfoBox = ({
  issues,
  sx,
}: {
  issues?: MetadataIssue[];
  sx?: SxProps;
}) => {
  const { t } = useTranslation();
  const warnings = getMetadataWarnings(issues);

  if (warnings.length === 0) return null;

  return (
    <Box
      data-testid="metadata-warning"
      sx={{
        mb: 4,
        p: 2,
        borderRadius: 2,
        border: "1px solid",
        borderColor: "lightOrange",
        backgroundColor: "rgba(255, 203, 173, 0.15)",
        ...sx,
      }}
    >
      <Typography
        sx={{ color: "orangeDark", fontWeight: 500, mb: 0.5 }}
        data-testid="metadata-warning-title"
      >
        {t("metadataIssues.warningTitle")}
      </Typography>
      <Box component="ul" sx={{ color: "orangeDark", my: 0.5, pl: 3 }}>
        {warnings.map((issue) => (
          <Typography
            component="li"
            key={`${issue.field}-${issue.rule}`}
            variant="body2"
            sx={{ color: "orangeDark" }}
          >
            {getMetadataIssueMessage(issue)}
          </Typography>
        ))}
      </Box>
      <Typography variant="body2" sx={{ color: "orangeDark" }}>
        {t("metadataIssues.warningDescription")}
      </Typography>
    </Box>
  );
};
