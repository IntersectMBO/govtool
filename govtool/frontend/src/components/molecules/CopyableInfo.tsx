import { Box, Typography } from "@mui/material";

import { CopyButton } from "@atoms";
import { Card } from "./Card";
import { gray } from "@/consts";

type CopyableInfoProps = {
  dataTestId?: string;
  label: string;
  value: string;
};

export const CopyableInfo = ({
  dataTestId,
  label,
  value,
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
    }}
  >
    <Box sx={{ display: "flex", justifyContent: "space-between" }}>
      <Typography color={gray.c300} fontSize={12} fontWeight={500}>
        {label}
      </Typography>
      <CopyButton text={value} />
    </Box>
    <Box display="flex" flexDirection="row" alignItems="center">
      <Typography
        textOverflow="ellipsis"
        overflow="hidden"
        fontSize={14}
        fontWeight={500}
        maxWidth="calc(100% - 1.5rem)"
      >
        {value}
      </Typography>
    </Box>
  </Card>
);
