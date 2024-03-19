import { useLocation, useNavigate } from "react-router-dom";
import { Box } from "@mui/material";

import { Background, Button, Typography } from "@atoms";
import { IMAGES, PATHS } from "@consts";
import { useCardano } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";

const IMAGE_SIZE = 748;

export function ErrorPage({
  errorCode = 404,
  errorDescription = "This page is not available",
  isButton = true,
}: {
  errorCode?: string | number;
  errorDescription?: string;
  isButton?: boolean;
}) {
  const navigate = useNavigate();
  const { isEnabled } = useCardano();
  const { screenWidth } = useScreenDimension();
  const { state } = useLocation();
  const { t } = useTranslation();

  return (
    <Background>
      <Box
        display="flex"
        flexDirection="column"
        justifyContent="center"
        minHeight="100vh"
        overflow="hidden"
        paddingX={screenWidth < 1024 ? 4 : 22.5}
        position="relative"
      >
        <Box>
          <Typography
            fontSize={57}
            fontWeight={700}
            lineHeight="64px"
            sx={{ whiteSpace: "nowrap" }}
          >
            {t("errorPage.whoops")}
          </Typography>
          <Typography sx={{ marginTop: 1 }} variant="headline3">
            {state && state.errorCode === 500
              ? t("errorPage.serverError")
              : errorDescription}
          </Typography>
          <Typography fontWeight={400} sx={{ marginY: 4.25 }} variant="title2">
            {t("errorPage.error")}
            {state ? state.errorCode : errorCode}
          </Typography>
          {isButton && (
            <Button size="extraLarge" onClick={() => navigate(PATHS.home)}>
              {isEnabled
                ? t("errorPage.backToDashboard")
                : t("errorPage.backToHomepage")}
            </Button>
          )}
        </Box>
        <Box
          flex={1}
          paddingY={3}
          position="absolute"
          right={screenWidth < 1024 ? -400 : -50}
          zIndex={-1}
        >
          <img
            height={IMAGE_SIZE}
            src={IMAGES.errorPageImage}
            width={IMAGE_SIZE}
          />
        </Box>
      </Box>
    </Background>
  );
}
