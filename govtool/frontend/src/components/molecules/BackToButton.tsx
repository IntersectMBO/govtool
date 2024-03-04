import { Box } from "@mui/material";
import ArrowBackIosIcon from "@mui/icons-material/ArrowBackIos";

import { Typography } from "@atoms";

import { BackToLinkProps } from "./types";

export const BackToButton = ({ label, onClick, sx }: BackToLinkProps) => {
  return (
    <Box
      data-testid="back-to-dashboard-link"
      sx={{
        alignItems: "center",
        cursor: "pointer",
        display: "flex",
        ...sx,
      }}
      onClick={onClick}
    >
      <ArrowBackIosIcon color="primary" sx={{ fontSize: 14 }} />
      <Typography color="primary" fontWeight={400} variant="body2">
        {label}
      </Typography>
    </Box>
  );
};
