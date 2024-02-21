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

export const CenteredBoxBottomButtons = ({
  onBackButton,
  onActionButton,
  isLoading,
  backButtonText,
  actionButtonText,
}: Props) => {
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  const renderBackButton = useMemo(() => {
    return (
      <Button
        data-testid={"back-button"}
        onClick={onBackButton}
        size="extraLarge"
        sx={{
          px: 6,
          width: isMobile ? "100%" : "auto",
        }}
        variant="outlined"
      >
        {backButtonText ?? t("cancel")}
      </Button>
    );
  }, [isMobile]);

  const renderActionButton = useMemo(() => {
    return (
      <LoadingButton
        data-testid={"register-button"}
        isLoading={isLoading}
        onClick={onActionButton}
        sx={{
          borderRadius: 50,
          textTransform: "none",
          px: 6,
          width: isMobile ? "100%" : "auto",
          height: 48,
          fontSize: 16,
        }}
        variant="contained"
      >
        {actionButtonText ?? t("continue")}
      </LoadingButton>
    );
  }, [isLoading, isMobile]);

  return (
    <Box
      display="flex"
      flexDirection={isMobile ? "column" : "row"}
      justifyContent="space-around"
      mt={6}
    >
      {isMobile ? renderActionButton : renderBackButton}
      <Box px={2} py={isMobile ? 1.5 : 0} />
      {isMobile ? renderBackButton : renderActionButton}
    </Box>
  );
};
