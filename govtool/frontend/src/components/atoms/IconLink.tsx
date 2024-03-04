import { Box } from "@mui/material";

import { Typography } from "@atoms";
import { openInNewTab } from "@utils";
import { ICONS } from "@consts";

type IconLinkProps = {
  label: string;
  navTo: string;
  isSmall?: boolean;
};

export const IconLink = ({ label, navTo, isSmall }: IconLinkProps) => {
  const openLink = () => openInNewTab(navTo);

  return (
    <Box
      onClick={openLink}
      sx={{
        alignItems: "center",
        cursor: "pointer",
        display: "flex",
        flexDirection: "row",
        overflow: "hidden",
      }}
    >
      <img alt="link" height={16} src={ICONS.link} width={16} />
      <Typography
        color="primary"
        fontWeight={400}
        sx={{
          ml: 0.5,
          overflow: "hidden",
          textOverflow: "ellipsis",
          ...(isSmall && {
            fontSize: 14,
            lineHeight: "20px",
          }),
        }}
      >
        {label}
      </Typography>
    </Box>
  );
};
