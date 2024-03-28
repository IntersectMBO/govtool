import { FC, PropsWithChildren } from "react";
import { Box } from "@mui/material";

import { useScreenDimension, useTranslation } from "@hooks";
import { Button } from "@atoms";

type VoteContextWrapperProps = {
  onContinue: () => void;
  isContinueDisabled?: boolean;
  onCancel: () => void;
};

export const VoteContextWrapper: FC<
  PropsWithChildren<VoteContextWrapperProps>
> = ({ onContinue, isContinueDisabled, onCancel, children }) => {
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
      <Box
        sx={{
          display: "flex",
          justifyContent: "space-between",
          marginTop: "40px",
          ...(isMobile && { flexDirection: "column-reverse", gap: 3 }),
        }}
      >
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
      </Box>
    </>
  );
};
