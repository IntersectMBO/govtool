import { Avatar, Box, SxProps } from "@mui/material";

import { Typography } from "@atoms";
import { MetadataValidationStatus } from "@models";
import {
  getBase64ImageDetails,
  getMetadataDataMissingStatusTranslation,
} from "@/utils";
import { ICONS } from "@/consts";
import { Share } from "./Share";
import { useScreenDimension } from "@/hooks";

type DataMissingHeaderProps = {
  isDataMissing: MetadataValidationStatus | null;
  title?: string;
  titleStyle?: SxProps;
  isDRep?: boolean;
  image?: string | null;
  shareLink?: string;
};

export const DataMissingHeader = ({
  title,
  isDataMissing,
  titleStyle,
  isDRep,
  image,
  shareLink,
}: DataMissingHeaderProps) => {
  const base64Image = getBase64ImageDetails(image ?? "");
  const { screenWidth } = useScreenDimension();

  return (
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
            xxs: "column",
            md: "row",
          },
          alignItems: {
            md: "center",
          },
          display: "flex",
        }}
      >
        {isDRep && (
          <Avatar
            alt="drep-image"
            src={
              (base64Image.isValidBase64Image
                ? `${base64Image.base64Prefix}${image}`
                : image) || ICONS.defaultDRepIcon
            }
            sx={{ width: 80, height: 80 }}
            data-testid="drep-image"
          />
        )}
        <Typography
          sx={{
            ...(isDRep && { ml: { md: 3 } }),
            ...(isDRep && { mt: { xxs: 2, md: 0 } }),
            textOverflow: "ellipsis",
            fontWeight: 600,
            ...(isDataMissing && { color: "errorRed" }),
            ...titleStyle,
          }}
          variant="title2"
          component="h1"
        >
          {(isDataMissing &&
            getMetadataDataMissingStatusTranslation(
              isDataMissing as MetadataValidationStatus,
            )) ||
            title}
        </Typography>
      </Box>
      {screenWidth >= 1020 && (
        <Share link={shareLink || window.location.href} />
      )}
    </Box>
  );
};
