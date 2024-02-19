import { useCallback } from "react";
import { useNavigate } from "react-router-dom";
import { Trans } from "react-i18next";
import { Box, Link } from "@mui/material";

import { Button, Typography } from "@atoms";
import { IMAGES, PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";
import { openInNewTab } from "@utils";

export const Hero = () => {
  const { isEnabled } = useCardano();
  const { openModal } = useModal();
  const navigate = useNavigate();
  const { isMobile, screenWidth, pagePadding } = useScreenDimension();
  const { t } = useTranslation();
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

  const onClickVotingPower = useCallback(
    () =>
      openInNewTab("https://docs.sanchogov.tools/faqs/what-is-voting-power"),
    []
  );

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
          sx={{
            maxWidth: 630,
            my: 4,
            ...(isMobile ? {} : { whiteSpace: "pre-line" }),
          }}
          variant={isMobile ? "body2" : "title2"}
        >
          <Trans
            i18nKey={
              screenWidth < 1024
                ? "hero.description.mobile"
                : "hero.description.wide"
            }
            components={[
              <Link
                data-testid="voting-power-link"
                onClick={onClickVotingPower}
                sx={{
                  cursor: "pointer",
                }}
              ></Link>,
            ]}
          ></Trans>
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
