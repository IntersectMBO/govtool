import { Avatar, Box, SxProps } from "@mui/material";

import { Typography } from "@atoms";
import { MetadataValidationStatus } from "@models";
import { getMetadataDataMissingStatusTranslation } from "@/utils";
import { ICONS } from "@/consts";

type DataMissingHeaderProps = {
  isDataMissing: MetadataValidationStatus | null;
  title?: string;
  titleStyle?: SxProps;
  isDRep?: boolean;
  image?: string | null;
};

export const DataMissingHeader = ({
  title,
  isDataMissing,
  titleStyle,
  isDRep,
  image,
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
        flexDirection: {
          sm: "column",
          lg: "row",
        },
        alignItems: {
          lg: "center",
        },
        display: "flex",
      }}
    >
      {isDRep && (
        <Avatar
          alt="drep-image"
          src={image || ICONS.defaultDRepIcon}
          sx={{ width: 80, height: 80 }}
          data-testid="drep-image"
        />
      )}
      <Typography
        sx={{
          ...(isDRep && { ml: { lg: 3 } }),
          ...(isDRep && { mt: { sm: 2, lg: 0 } }),
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
