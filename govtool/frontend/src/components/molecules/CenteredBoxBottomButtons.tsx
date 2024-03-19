import { useMemo } from "react";
import { Box } from "@mui/material";

import { Button, LoadingButton } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";

interface Props {
  onBackButton: () => void;
  onActionButton: () => void;
  isLoading?: boolean;
  backButtonText?: string;
  actionButtonText?: string;
}

export function CenteredBoxBottomButtons({
  onBackButton,
  onActionButton,
  isLoading,
  backButtonText,
  actionButtonText,
}: Props) {
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  const renderBackButton = useMemo(() => (
    <Button
      data-testid="back-button"
      onClick={onBackButton}
      size="extraLarge"
      sx={{
        px: 6,
      }}
      variant="outlined"
    >
      {backButtonText ?? t("cancel")}
    </Button>
  ), [isMobile]);

  const renderActionButton = useMemo(() => (
    <LoadingButton
      data-testid="register-button"
      isLoading={isLoading}
      onClick={onActionButton}
      sx={{
        px: 6,
        height: 48,
        fontSize: 16,
      }}
      variant="contained"
    >
      {actionButtonText ?? t("continue")}
    </LoadingButton>
  ), [isLoading, isMobile]);

  return (
    <Box
      display="flex"
      flexDirection={isMobile ? "column-reverse" : "row"}
      justifyContent="space-around"
      mt={6}
    >
      {renderBackButton}
      <Box px={2} py={isMobile ? 1.5 : 0} />
      {renderActionButton}
    </Box>
  );
}
