import { useCallback, useMemo } from "react";
import { useNavigate } from "react-router-dom";
import { Box, Link } from "@mui/material";
import ArrowBackIosIcon from "@mui/icons-material/ArrowBackIos";

import { Button, Typography } from "@atoms";
import { PATHS } from "@consts";
import { useScreenDimension, useTranslation } from "@hooks";
import { theme } from "@/theme";

import { BgCardProps } from "./types";

export const BgCard = ({
  actionButtonLabel,
  children,
  onClickBackButton,
  onClickActionButton,
  sx,
  title,
}: BgCardProps) => {
  const {
    palette: { boxShadow2 },
  } = theme;
  const { isMobile, screenWidth } = useScreenDimension();
  const navigate = useNavigate();
  const { t } = useTranslation();

  const navigateToDashboard = useCallback(
    () => navigate(PATHS.dashboard),
    [navigate]
  );

  const renderBackButton = useMemo(() => {
    return (
      <Button
        data-testid="back-button"
        onClick={onClickBackButton ?? navigateToDashboard}
        size="extraLarge"
        sx={{
          px: 6,
          width: isMobile ? "100%" : "auto",
        }}
        variant="outlined"
      >
        {t("cancel")}
      </Button>
    );
  }, [isMobile]);

  const renderContinueButton = useMemo(() => {
    return (
      <Button
        data-testid="retire-button"
        onClick={onClickActionButton}
        size="extraLarge"
        sx={{
          px: 6,
          width: isMobile ? "100%" : "auto",
        }}
        variant="contained"
      >
        {actionButtonLabel}
      </Button>
    );
  }, [isMobile]);

  return (
    <Box
      height={isMobile ? "100%" : "auto"}
      sx={{
        alignItems: screenWidth >= 768 ? "center" : "inherit",
        marginTop: screenWidth < 1440 ? "97px" : "137px",
        display: screenWidth < 1440 ? "flex" : "grid",
        ...(screenWidth < 1440 && {
          flexDirection: "column",
        }),
        ...(screenWidth >= 1440 && { gridTemplateColumns: "1fr auto 1fr" }),
      }}
    >
      {isMobile && (
        <Box borderBottom="1px solid white">
          <Typography
            variant="title1"
            sx={{
              ml: 2,
              my: 3.25,
            }}
          >
            {title}
          </Typography>
        </Box>
      )}
      <Link
        data-testid="back-to-list-link"
        sx={{
          alignItems: "center",
          alignSelf: "flex-start",
          cursor: "pointer",
          display: "flex",
          justifyContent: "flex-start",
          ml: screenWidth < 1440 ? 2 : 5,
          mt: screenWidth < 1440 ? 3 : "none",
          textDecoration: "none",
        }}
        onClick={navigateToDashboard}
      >
        <ArrowBackIosIcon sx={{ fontSize: 14 }} />
        <Typography color="primary" fontWeight={400} variant="body2">
          {t("backToDashboard")}
        </Typography>
      </Link>
      <Box
        borderRadius="20px"
        boxShadow={isMobile ? "" : `2px 2px 20px 0px ${boxShadow2}`}
        height="auto"
        maxWidth={screenWidth > 768 ? 600 : undefined}
        px={isMobile ? 2 : 18.75}
        pt={isMobile ? 6 : 10}
        pb={3}
        sx={sx}
      >
        <Box display="flex" flexDirection="column">
          {children}
        </Box>
        <Box
          display="flex"
          flexDirection={isMobile ? "column" : "row"}
          justifyContent="space-between"
        >
          {isMobile ? renderContinueButton : renderBackButton}
          <Box px={2} py={isMobile ? 1.5 : 0} />
          {isMobile ? renderBackButton : renderContinueButton}
        </Box>
      </Box>
    </Box>
  );
};
