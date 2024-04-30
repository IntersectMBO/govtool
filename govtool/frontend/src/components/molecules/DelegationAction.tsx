import { Box } from "@mui/material";
import ArrowForwardIosIcon from "@mui/icons-material/ArrowForwardIos";

import { Typography } from "@atoms";
import { gray } from "@consts";
import { useTranslation } from "@hooks";

import { Card } from "./Card";
import { DirectVoterActionProps } from "./types";

export const DelegationAction = ({
  dRepId,
  onClickArrow,
  sx,
}: DirectVoterActionProps) => {
  const { t } = useTranslation();

  return (
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
        ...sx,
      }}
    >
      <Box sx={{ width: "90%" }}>
        <Typography fontWeight={600} variant="body2">
          {t("dashboard.cards.drepName")}
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
      <ArrowForwardIosIcon
        color="primary"
        onClick={onClickArrow}
        sx={{ cursor: "pointer", height: 16, width: 24 }}
      />
    </Card>
  );
};
