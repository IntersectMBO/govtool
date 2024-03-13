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
  const { isMobile, screenWidth } = useScreenDimension();
  const { t } = useTranslation();
  const IMAGE_SIZE = screenWidth < 640 ? 300 : screenWidth < 860 ? 400 : 600;

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
      marginTop={16}
      flexDirection="row"
      overflow="visible"
      position="relative"
      px={screenWidth < 640 ? 3 : screenWidth < 1512 ? 10 : 14}
    >
      <Box alignItems="center" flex={1} height="min-content">
        <Typography
          variant={screenWidth < 860 ? "headline2" : "headline1"}
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
            whiteSpace: "pre-line",
          }}
          variant="title2"
        >
          <Trans
            i18nKey="hero.description"
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
        right={
          screenWidth >= 1728
            ? IMAGE_SIZE / 8
            : screenWidth >= 1512
            ? -(IMAGE_SIZE / 12)
            : screenWidth >= 860
            ? -(IMAGE_SIZE / 8)
            : -(IMAGE_SIZE / 4)
        }
        top={-80}
        zIndex={-1}
      >
        <img src={IMAGES.heroImage} width={IMAGE_SIZE} height={IMAGE_SIZE} />
      </Box>
    </Box>
  );
};
