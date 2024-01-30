import { useNavigate } from "react-router-dom";
import { Box, Button, Typography } from "@mui/material";

import { PATHS } from "@consts";
import { useCardano } from "@context";
import { theme } from "@/theme";
import { useTranslation } from "@hooks";

export const WalletInfoCard = () => {
  const { address, disconnectWallet, isMainnet } = useCardano();
  const navigate = useNavigate();
  const { t } = useTranslation();
  const {
    palette: { lightBlue },
  } = theme;

  return (
    address && (
      <Box
        border={1}
        borderColor={lightBlue}
        py={1.5}
        px={2}
        borderRadius={3}
        position={"relative"}
      >
        <Box
          bgcolor={"rgb(214, 226, 255)"}
          px={1.2}
          py={0.5}
          borderRadius={100}
          sx={{
            position: "absolute",
            top: -15,
            right: 10,
          }}
        >
          <Typography variant="caption" color={"primary"} fontWeight={500}>
            {isMainnet ? "mainnet" : "testnet"}
          </Typography>
        </Box>
        <Typography color="gray" fontSize={12} fontWeight={500}>
          {t("wallet.connectedWallet")}
        </Typography>
        <Box
          display="flex"
          flexDirection="row"
          justifyContent="space-between"
          mt={1}
          alignItems="center"
        >
          <Typography
            width={"50vw"}
            textOverflow={"ellipsis"}
            overflow={"hidden"}
            fontSize={14}
            fontWeight={400}
          >
            {address}
          </Typography>
          <Box>
            <Button
              data-testid={"disconnect-button"}
              variant="text"
              onClick={async () => {
                await disconnectWallet();
                navigate(PATHS.home);
                window.location.reload();
              }}
              sx={{ textTransform: "none", fontWeight: 500 }}
            >
              {t("wallet.disconnect")}
            </Button>
          </Box>
        </Box>
      </Box>
    )
  );
};
