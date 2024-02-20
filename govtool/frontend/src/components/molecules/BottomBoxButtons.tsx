import { useMemo } from "react";
import { Box } from "@mui/material";

import { Button, LoadingButton } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";

interface Props {
  leftButtonAction: () => void;
  rightButtonAction: () => void;
  rightButtonIsLoading?: boolean;
  leftButtonText?: string;
  rightButtonText?: string;
}

export const BottomBoxButtons = ({
  leftButtonAction,
  rightButtonAction,
  rightButtonIsLoading,
  leftButtonText,
  rightButtonText,
}: Props) => {
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  const renderBackButton = useMemo(() => {
    return (
      <Button
        data-testid={"back-button"}
        onClick={leftButtonAction}
        size="extraLarge"
        sx={{
          px: 6,
          width: isMobile ? "100%" : "auto",
        }}
        variant="outlined"
      >
        {leftButtonText ?? t("cancel")}
      </Button>
    );
  }, [isMobile]);

  const renderRegisterButton = useMemo(() => {
    return (
      <LoadingButton
        data-testid={"register-button"}
        isLoading={rightButtonIsLoading}
        onClick={rightButtonAction}
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
        {rightButtonText ?? t("continue")}
      </LoadingButton>
    );
  }, [rightButtonIsLoading, isMobile]);

  return (
    <Box
      display="flex"
      flexDirection={isMobile ? "column" : "row"}
      justifyContent="space-around"
      mt={6}
    >
      {isMobile ? renderRegisterButton : renderBackButton}
      <Box px={2} py={isMobile ? 1.5 : 0} />
      {isMobile ? renderBackButton : renderRegisterButton}
    </Box>
  );
};
