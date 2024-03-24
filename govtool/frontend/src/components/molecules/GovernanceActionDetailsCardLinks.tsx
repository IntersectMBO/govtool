import { Box } from "@mui/material";

import { IconLink, Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";

// TODO: When BE is ready, pass links as props
const LINKS = [
  "https://docs.sanchogov.tools/support/get-help-in-discord",
  "https://docs.sanchogov.tools/how-to-use-the-govtool/prerequsites",
  "https://docs.sanchogov.tools/faqs",
  "https://docs.sanchogov.tools/",
];

export const GovernanceActionDetailsCardLinks = () => {
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  return (
    <>
      <Typography
        sx={{
          fontSize: 14,
          fontWeight: 600,
          lineHeight: "20px",
          color: "neutralGray",
          overflow: "hidden",
          textOverflow: "ellipsis",
          whiteSpace: "nowrap",
          mb: 2,
        }}
        data-testid="supporting-links"
      >
        {t("govActions.supportingLinks")}
      </Typography>
      <Box
        sx={{
          display: "grid",
          gridTemplateColumns: isMobile ? undefined : "1fr 1fr",
          columnGap: 2,
          rowGap: 2,
        }}
      >
        {LINKS.map((link) => (
          <IconLink key={link} label={link} navTo={link} isSmall />
        ))}
      </Box>
    </>
  );
};
