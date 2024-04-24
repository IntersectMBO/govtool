import { Box, Link } from "@mui/material";

import { Typography } from "@atoms";
import { useTranslation } from "@hooks";
import { MetadataValidationStatus } from "@models";
import { openInNewTab } from "@utils";

export const DataMissingInfoBox = ({
  isDataMissing,
  isInProgress,
  isSubmitted,
}: {
  isDataMissing: boolean | MetadataValidationStatus;
  isInProgress?: boolean;
  isSubmitted?: boolean;
}) => {
  const { t } = useTranslation();

  const gaMetadataErrorMessage = {
    [MetadataValidationStatus.URL_NOT_FOUND]: t(
      "errors.gAMetadata.message.dataMissing",
    ),
    [MetadataValidationStatus.INVALID_JSONLD]: t(
      "errors.gAMetadata.message.incorrectFormat",
    ),
    [MetadataValidationStatus.INVALID_HASH]: t(
      "errors.gAMetadata.message.notVerifiable",
    ),
    [MetadataValidationStatus.INCORRECT_FORMAT]: t(
      "errors.gAMetadata.message.incorrectFormat",
    ),
  }[isDataMissing as MetadataValidationStatus];

  const gaMetadataErrorDescription = {
    [MetadataValidationStatus.URL_NOT_FOUND]: t(
      "errors.gAMetadata.description.dataMissing",
    ),
    [MetadataValidationStatus.INVALID_JSONLD]: t(
      "errors.gAMetadata.description.incorrectFormat",
    ),
    [MetadataValidationStatus.INVALID_HASH]: t(
      "errors.gAMetadata.description.notVerifiable",
    ),
    [MetadataValidationStatus.INCORRECT_FORMAT]: t(
      "errors.gAMetadata.description.incorrectFormat",
    ),
  }[isDataMissing as MetadataValidationStatus];

  return isDataMissing && !isSubmitted && !isInProgress ? (
    <Box
      sx={{
        mb: 4,
        pr: 6,
      }}
    >
      <Typography
        sx={{
          fontSize: "18px",
          fontWeight: 500,
          color: "errorRed",
          mb: 0.5,
        }}
      >
        {gaMetadataErrorMessage}
      </Typography>
      <Typography
        sx={{
          fontWeight: 400,
          color: "errorRed",
          mb: 0.5,
        }}
      >
        {gaMetadataErrorDescription}
      </Typography>
      <Link
        onClick={() =>
          // TODO: Add the correct link
          openInNewTab(
            "https://docs.sanchogov.tools/how-to-use-the-govtool/getting-started/get-a-compatible-wallet",
          )
        }
        sx={{
          fontFamily: "Poppins",
          fontSize: "16px",
          lineHeight: "24px",
          cursor: "pointer",
        }}
      >
        {t("learnMore")}
      </Link>
    </Box>
  ) : null;
};
