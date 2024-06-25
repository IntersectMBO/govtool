import { Box, Link, SxProps } from "@mui/material";

import { Typography } from "@atoms";
import { useTranslation } from "@hooks";
import { MetadataValidationStatus } from "@models";

export const DataMissingInfoBox = ({
  isDataMissing,
  isInProgress,
  isSubmitted,
  isDrep = false,
  sx,
}: {
  isDataMissing: MetadataValidationStatus | null;
  isInProgress?: boolean;
  isSubmitted?: boolean;
  isDrep?: boolean;
  sx?: SxProps;
}) => {
  const { t } = useTranslation();

  const gaMetadataErrorMessage = {
    [MetadataValidationStatus.URL_NOT_FOUND]: isDrep
      ? t("errors.dRep.message.dataMissing")
      : t("errors.gAMetadata.message.dataMissing"),
    [MetadataValidationStatus.INVALID_JSONLD]: isDrep
      ? t("errors.dRep.message.incorrectFormat")
      : t("errors.gAMetadata.message.incorrectFormat"),
    [MetadataValidationStatus.INVALID_HASH]: isDrep
      ? t("errors.dRep.message.notVerifiable")
      : t("errors.gAMetadata.message.notVerifiable"),
    [MetadataValidationStatus.INCORRECT_FORMAT]: isDrep
      ? t("errors.dRep.message.incorrectFormat")
      : t("errors.gAMetadata.message.incorrectFormat"),
  }[isDataMissing as MetadataValidationStatus];

  const gaMetadataErrorDescription = {
    [MetadataValidationStatus.URL_NOT_FOUND]: isDrep
      ? t("errors.dRep.description.dataMissing")
      : t("errors.gAMetadata.description.dataMissing"),
    [MetadataValidationStatus.INVALID_JSONLD]: isDrep
      ? t("errors.dRep.description.incorrectFormat")
      : t("errors.gAMetadata.description.incorrectFormat"),
    [MetadataValidationStatus.INVALID_HASH]: isDrep
      ? t("errors.dRep.description.notVerifiable")
      : t("errors.gAMetadata.description.notVerifiable"),
    [MetadataValidationStatus.INCORRECT_FORMAT]: isDrep
      ? t("errors.dRep.description.incorrectFormat")
      : t("errors.gAMetadata.description.incorrectFormat"),
  }[isDataMissing as MetadataValidationStatus];

  return isDataMissing && !isSubmitted && !isInProgress ? (
    <Box
      sx={{
        mb: 4,
        pr: 6,
        ...sx,
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
        // TODO: Add the correct link
        href="https://docs.sanchogov.tools/how-to-use-the-govtool/getting-started/get-a-compatible-wallet"
        target="_blank"
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
