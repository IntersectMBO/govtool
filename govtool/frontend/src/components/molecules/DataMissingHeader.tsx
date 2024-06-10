import { Box } from "@mui/material";

import { Typography } from "@atoms";
import { Share } from "@molecules";
import { MetadataValidationStatus } from "@models";
import { getMetadataDataMissingStatusTranslation } from "@/utils";

type DataMissingHeaderProps = {
  isDataMissing: MetadataValidationStatus | null;
  shareLink?: string;
  title?: string;
};

export const DataMissingHeader = ({
  title,
  isDataMissing,
  shareLink,
}: DataMissingHeaderProps) => (
  <Box
    sx={{
      display: "grid",
      gridTemplateColumns: "1fr auto",
      gap: 2,
      alignItems: "center",
      mb: 2,
    }}
    data-testid="governance-action-details-card-header"
  >
    <Box
      sx={{
        display: "flex",
        alignItems: "center",
      }}
    >
      <Typography
        sx={{
          overflow: "hidden",
          textOverflow: "ellipsis",
          whiteSpace: "nowrap",
          fontWeight: 600,
          ...(isDataMissing && { color: "errorRed" }),
        }}
        variant="title2"
      >
        {(isDataMissing &&
          getMetadataDataMissingStatusTranslation(
            isDataMissing as MetadataValidationStatus,
          )) ||
          title}
      </Typography>
    </Box>
    {shareLink && <Share link={shareLink} />}
  </Box>
);
