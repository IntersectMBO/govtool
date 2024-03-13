import { useNavigate } from "react-router-dom";
import { Box, Button, Typography } from "@mui/material";

import { PATHS } from "@consts";
import { useCardano } from "@context";
import { useTranslation } from "@hooks";

export const WalletInfoCard = () => {
  const { address, disconnectWallet } = useCardano();
  const navigate = useNavigate();
  const { t } = useTranslation();

  const onClickDisconnect = async () => {
    await disconnectWallet();
    navigate(PATHS.home);
    window.location.reload();
  };

  return (
    address && (
      <Box
        sx={{
          border: 1,
          borderColor: "lightBlue",
          borderRadius: 3,
          px: 1.75,
          py: 1.5,
          position: "relative",
        }}
      >
        <Typography sx={{ color: "#ADAEAD", fontSize: 12, fontWeight: 500 }}>
          {t("wallet.connectedWallet")}
        </Typography>
        <Box sx={{ alignItems: "center", display: "flex" }}>
          <Typography
            sx={{
              flex: 1,
              fontSize: 14,
              fontWeight: 400,
              overflow: "hidden",
              textOverflow: "ellipsis",
              width: 10,
            }}
          >
            {address}
          </Typography>
          <Button
            data-testid={"disconnect-button"}
            variant="text"
            onClick={onClickDisconnect}
          >
            {t("wallet.disconnect")}
          </Button>
        </Box>
      </Box>
    )
  );
};
