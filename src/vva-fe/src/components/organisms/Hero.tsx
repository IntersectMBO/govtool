import { Box } from "@mui/material";
import { useNavigate } from "react-router-dom";

import { Button, Typography } from "@atoms";
import { IMAGES, PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import { useScreenDimension } from "@hooks";
import { usei18n } from "@translations";

export const Hero = () => {
  const { isEnabled } = useCardano();
  const { openModal } = useModal();
  const navigate = useNavigate();
  const { isMobile, screenWidth, pagePadding } = useScreenDimension();
  const { t } = usei18n();
  const IMAGE_SIZE =
    screenWidth < 768
      ? 140
      : screenWidth < 1024
      ? 400
      : screenWidth < 1440
      ? 500
      : screenWidth < 1920
      ? 600
      : 720;

  return (
    <Box
      alignItems="center"
      display="flex"
      flex={1}
      flexDirection="row"
      height={screenWidth < 1024 ? "70vh" : "75vh"}
      overflow="visible"
      position="relative"
      px={pagePadding}
    >
      <Box alignItems="center" flex={1} height="min-content">
        <Typography
          variant={screenWidth < 1024 ? "headline2" : "headline1"}
          sx={{ whiteSpace: "pre-line" }}
          {...(screenWidth < 375 && { fontSize: 45 })}
        >
          {t("hero.headline")}
        </Typography>
        <Typography
          fontWeight={400}
          sx={{ my: 4, ...(isMobile ? {} : { whiteSpace: "pre-line" }) }}
          variant={isMobile ? "body2" : "title2"}
        >
          {t("hero.description")}
        </Typography>
        <Button
          data-testid="hero-connect-wallet-button"
          onClick={() => {
            if (isEnabled) {
              navigate(PATHS.dashboard);
            } else {
              openModal({ type: "chooseWallet" });
            }
          }}
          size={isMobile ? "medium" : "extraLarge"}
        >
          {t("hero.connectWallet")}
        </Button>
      </Box>
      <Box
        flex={1}
        position="absolute"
        right={isMobile ? -30 : screenWidth < 1920 ? -50 : 50}
        top={screenWidth < 1024 ? 0 : undefined}
        zIndex={-1}
      >
        <img src={IMAGES.heroImage} width={IMAGE_SIZE} height={IMAGE_SIZE} />
      </Box>
    </Box>
  );
};
