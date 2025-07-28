import { FC, PropsWithChildren } from "react";
import { Box } from "@mui/material";

import { useScreenDimension, useTranslation } from "@hooks";
import { Button } from "@atoms";

type VoteContextWrapperProps = {
  onContinue?: () => void;
  isContinueDisabled?: boolean;
  showCancelButton? : boolean;
  onCancel: () => void;
  showContinueButton?: boolean;
  showAllButtons? : boolean;
};

export const VoteContextWrapper: FC<
  PropsWithChildren<VoteContextWrapperProps>
> = ({
  onContinue,
  isContinueDisabled,
  onCancel,
  showCancelButton = true,
  children,
  showContinueButton = true,
  showAllButtons = true
}) => {
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  return (
    <>
      <Box
        sx={{
          display: "flex",
          flexDirection: "column",
          alignItems: "center",
        }}
      >
        {children}
      </Box>
      {
        showAllButtons &&
      <Box
              sx={{
                display: "flex",
                justifyContent: "space-between",
                marginTop: "40px",
                ...(isMobile && { flexDirection: "column-reverse", gap: 3 }),
              }}
            >
              {
                showCancelButton &&
              <Button
                data-testid="cancel-modal-button"
                onClick={onCancel}
                size="large"
                sx={{
                  width: isMobile ? "100%" : "154px",
                }}
                variant="outlined"
      >
        {t("cancel")}
      </Button>
              }
      {showContinueButton && (
        <Button
          data-testid="confirm-modal-button"
          disabled={isContinueDisabled}
          onClick={onContinue}
          size="large"
          sx={{
            width: isMobile ? "100%" : "154px",
          }}
          variant="contained"
        >
          {t("continue")}
        </Button>
      )}
      </Box>
      }
    </>
  );
};
