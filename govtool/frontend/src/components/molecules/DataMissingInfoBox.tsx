import { Box, Link } from "@mui/material";

import { Typography } from "@atoms";
import { useTranslation } from "@hooks";
import { GAMetedataErrors, openInNewTab } from "@utils";

export const DataMissingInfoBox = ({
  isDataMissing,
  isInProgress,
  isSubmitted,
}: {
  isDataMissing: boolean | GAMetedataErrors;
  isInProgress?: boolean;
  isSubmitted?: boolean;
}) => {
  const { t } = useTranslation();

  const gaMetadataErrorMessage = {
    [GAMetedataErrors.DATA_MISSING]: t("errors.gAMetadata.message.dataMissing"),
    [GAMetedataErrors.INCORRECT_FORMAT]: t(
      "errors.gAMetadata.message.incorrectFormat",
    ),
    [GAMetedataErrors.NOT_VERIFIABLE]: t(
      "errors.gAMetadata.message.notVerifiable",
    ),
  }[isDataMissing as GAMetedataErrors];

  const gaMetadataErrorDescription = {
    [GAMetedataErrors.DATA_MISSING]: t(
      "errors.gAMetadata.description.dataMissing",
    ),
    [GAMetedataErrors.INCORRECT_FORMAT]: t(
      "errors.gAMetadata.description.incorrectFormat",
    ),
    [GAMetedataErrors.NOT_VERIFIABLE]: t(
      "errors.gAMetadata.description.notVerifiable",
    ),
  }[isDataMissing as GAMetedataErrors];

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
