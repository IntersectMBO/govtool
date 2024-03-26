import { Box, Link } from "@mui/material";

import { Typography } from "@atoms";
import { useTranslation } from "@hooks";
import { openInNewTab } from "@utils";

export const DataMissingInfoBox = () => {
  const { t } = useTranslation();

  return (
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
        {/* TODO: Text to confirm/change */}
        The data that was originally used when this Governance Action was
        created has been formatted incorrectly.
      </Typography>
      <Typography
        sx={{
          fontWeight: 400,
          color: "errorRed",
          mb: 0.5,
        }}
      >
        {/* TODO: Text to confirm/change */}
        GovTool uses external sources for Governance Action data, and these
        sources are maintained by the proposers of the Actions. This error means
        that GovTool cannot locate the data on the URL specified when the
        Governance Action was originally posted.
      </Typography>
      <Link
        onClick={() =>
          // TODO: Add the correct link
          openInNewTab(
            "https://docs.sanchogov.tools/how-to-use-the-govtool/getting-started/get-a-compatible-wallet"
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
  );
};
