import { Box, SxProps } from "@mui/material";

import { Typography } from "@atoms";
import { MetadataValidationStatus } from "@models";
import { getMetadataDataMissingStatusTranslation } from "@/utils";

type DataMissingHeaderProps = {
  isDataMissing: MetadataValidationStatus | null;
  title?: string;
  titleStyle?: SxProps;
};

export const DataMissingHeader = ({
  title,
  isDataMissing,
  titleStyle,
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
          textOverflow: "ellipsis",
          fontWeight: 600,
          ...(isDataMissing && { color: "errorRed" }),
          ...titleStyle,
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
  </Box>
);
