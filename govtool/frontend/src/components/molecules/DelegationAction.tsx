import { Box } from "@mui/material";
import ArrowForwardIosIcon from "@mui/icons-material/ArrowForwardIos";

import { Typography } from "@atoms";
import { gray } from "@consts";

import { Card } from "./Card";
import { DirectVoterActionProps } from "./types";

export const DelegationAction = ({
  dRepId,
  drepName,
  onCardClick,
  sx,
}: DirectVoterActionProps) => (
  <Card
    border
    elevation={0}
    sx={{
      alignItems: "center",
      backgroundColor: (theme) => theme.palette.neutralWhite,
      borderColor: gray.c100,
      display: "flex",
      justifyContent: "space-between",
      px: 1.5,
      py: 1,
      cursor: "pointer",
      ...sx,
    }}
    onCardClick={onCardClick}
  >
    <Box sx={{ width: "90%" }}>
      <Typography fontWeight={600} variant="body2">
        {drepName}
      </Typography>
      <Typography
        sx={{
          mt: 0.5,
          overflow: "hidden",
          textOverflow: "ellipsis",
        }}
        variant="body2"
      >
        {dRepId}
      </Typography>
    </Box>
    <ArrowForwardIosIcon color="primary" sx={{ height: 16, width: 24 }} />
  </Card>
);
