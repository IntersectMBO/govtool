import { Box, Typography } from "@mui/material";

import { useCardano } from "@context";
import { CopyButton } from "@atoms";
import { useTranslation } from "@hooks";

export const DRepInfoCard = () => {
  const { dRepIDBech32 } = useCardano();
  const { t } = useTranslation();

  return (
    <Box border={1} borderColor="#D6E2FF" py={1} px={2} borderRadius={3}>
      <Box sx={{ display: "flex", justifyContent: "space-between" }}>
        <Typography color="gray" fontSize={12} fontWeight={500}>
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
    </Box>
  );
};
