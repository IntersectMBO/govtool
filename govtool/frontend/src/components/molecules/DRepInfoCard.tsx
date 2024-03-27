import { Box, Typography } from "@mui/material";

import { useCardano } from "@context";
import { CopyButton } from "@atoms";
import { useTranslation } from "@hooks";
import { Card } from "./Card";
import { gray } from "@/consts";

export const DRepInfoCard = () => {
  const { dRepIDBech32 } = useCardano();
  const { t } = useTranslation();

  return (
    <Card border elevation={0} sx={{ p: 1.5 }}>
      <Box sx={{ display: "flex", justifyContent: "space-between" }}>
        <Typography color={gray.c300} fontSize={12} fontWeight={500}>
          {t("myDRepId")}
        </Typography>
        <CopyButton text={dRepIDBech32} variant="blue" />
      </Box>
      <Box display="flex" flexDirection="row" mt={0.5} alignItems="center">
        <Typography
          data-testid="dRep-id-display"
          textOverflow="ellipsis"
          overflow="hidden"
          width="70vw"
          fontSize={14}
          fontWeight={500}
        >
          {dRepIDBech32}
        </Typography>
      </Box>
    </Card>
  );
};
