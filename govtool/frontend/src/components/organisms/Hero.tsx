import { useMemo } from "react";
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

  const paddingHorizontal = useMemo(() => {
    if (screenWidth < 640) return 3;
    if (screenWidth < 1512) return 9.375;
    if (screenWidth < 1728) return 14;
    if (screenWidth < 1920) return 27.375;
    if (screenWidth < 2560) return 39.375;
    return 49.25;
  }, [screenWidth]);

  const imageRightMargin = useMemo(() => {
    if (screenWidth <= 860) return -(IMAGE_SIZE / 4);
    if (screenWidth <= 1440) return -(IMAGE_SIZE / 15);
    if (screenWidth <= 1728) return screenWidth / 20;
    return screenWidth / 11;
  }, [screenWidth]);

  const onClickVotingPower = () =>
    openInNewTab("https://docs.gov.tools/faqs/what-is-voting-power");

  return (
    <Box
      alignItems="center"
      display="flex"
      flex={1}
      marginTop={16}
      flexDirection="row"
      overflow="visible"
      position="relative"
      px={paddingHorizontal}
    >
      <Box alignItems="center" flex={1} height="min-content">
        <Typography
          variant={screenWidth < 860 ? "headline2" : "headline1"}
          sx={{ whiteSpace: "pre-line" }}
          {...(screenWidth < 430 && { fontSize: 50 })}
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
              />,
            ]}
          />
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
        right={imageRightMargin}
        top={-80}
        zIndex={-1}
      >
        <img
          alt="hero"
          src={IMAGES.heroImage}
          width={IMAGE_SIZE}
          height={IMAGE_SIZE}
        />
      </Box>
    </Box>
  );
};
