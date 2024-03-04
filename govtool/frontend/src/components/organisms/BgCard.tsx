import { useCallback, useMemo } from "react";
import { useNavigate } from "react-router-dom";
import { Box } from "@mui/material";

import { Button, LoadingButton } from "@atoms";
import { PATHS } from "@consts";
import { useScreenDimension, useTranslation } from "@hooks";
import { theme } from "@/theme";

import { BgCardProps } from "./types";

export const BgCard = ({
  actionButtonLabel,
  backButtonLabel,
  children,
  isLoadingActionButton,
  isActionButtonDisabled,
  onClickBackButton,
  onClickActionButton,
  sx,
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
        }}
        variant="outlined"
      >
        {backButtonLabel ?? t("back")}
      </Button>
    );
  }, [isMobile]);

  const renderContinueButton = useMemo(() => {
    return (
      <LoadingButton
        data-testid="retire-button"
        disabled={isActionButtonDisabled}
        isLoading={isLoadingActionButton}
        onClick={onClickActionButton}
        size="extraLarge"
        sx={{
          px: 6,
        }}
        variant="contained"
      >
        {actionButtonLabel}
      </LoadingButton>
    );
  }, [
    actionButtonLabel,
    isActionButtonDisabled,
    isLoadingActionButton,
    isMobile,
  ]);

  return (
    <Box
      sx={{
        alignItems: screenWidth >= 768 ? "center" : "inherit",
        display: "flex",
        flex: 1,
        flexDirection: "column",
        height: isMobile ? "100%" : "auto",
      }}
    >
      <Box
        sx={{
          borderRadius: "20px",
          boxShadow: isMobile ? "" : `2px 2px 20px 0px ${boxShadow2}`,
          display: "flex",
          flex: isMobile ? 1 : undefined,
          flexDirection: "column",
          height: "auto",
          maxWidth: screenWidth > 768 ? 600 : undefined,
          mb: isMobile ? undefined : 3,
          pb: isMobile ? undefined : 10,
          pt: isMobile ? 6 : 10,
          px: isMobile ? 2 : 18.75,
          ...sx,
        }}
      >
        <Box sx={{ display: "flex", flex: 1, flexDirection: "column" }}>
          {children}
        </Box>
        <Box
          sx={{
            display: "flex",
            flexDirection: isMobile ? "column-reverse" : "row",
            gap: isMobile ? 3 : 0,
            justifyContent: "space-between",
          }}
        >
          {renderBackButton}
          {renderContinueButton}
        </Box>
      </Box>
    </Box>
  );
};
