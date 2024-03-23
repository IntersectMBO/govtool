import { Box } from "@mui/material";

import { IconLink, Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";

const LINKS = [
  "https://www.google.com",
  "https://www.google.com",
  "https://www.google.com",
  "https://www.google.com",
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
