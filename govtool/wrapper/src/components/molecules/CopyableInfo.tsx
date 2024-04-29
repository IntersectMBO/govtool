import { Box, SxProps, Typography } from "@mui/material";

import { CopyButton } from "@atoms";
import { gray } from "@consts";

import { Card } from "./Card";

type CopyableInfoProps = {
  dataTestId?: string;
  label: string;
  value: string;
  sx?: SxProps;
};

export const CopyableInfo = ({
  dataTestId,
  label,
  value,
  sx,
}: CopyableInfoProps) => (
  <Card
    border
    data-testid={dataTestId}
    elevation={0}
    sx={{
      px: 1.5,
      py: 1,
      borderColor: gray.c100,
      backgroundColor: (theme) => theme.palette.neutralWhite,
      ...sx,
    }}
  >
    <Typography color={gray.c300} fontSize={12} fontWeight={500}>
      {label}
    </Typography>
    <Box sx={{ display: "flex", justifyContent: "space-between" }}>
      <Typography
        textOverflow="ellipsis"
        overflow="hidden"
        fontSize={14}
        fontWeight={500}
        width="90%"
      >
        {value}
      </Typography>
      <CopyButton text={value} />
    </Box>
  </Card>
);
