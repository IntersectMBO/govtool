import { FC, PropsWithChildren } from "react";
import { Box } from "@mui/material";

import { useScreenDimension, useTranslation } from "@hooks";
import { Button } from "@atoms";

type VoteContextWrapperProps = {
  onContinue?: () => void;
  isVoteWithMetadata?: boolean;
  onCancel?: () => void;
  hideAllBtn?: boolean;
  useBackLabel?: boolean;
  useSubmitLabel?: boolean;
  onSkip?: () => void;
  continueLabel?: string;
  isApiLoading?:boolean;
  isContinueDisabled?:boolean
};

export const VoteContextWrapper: FC<
  PropsWithChildren<VoteContextWrapperProps>
> = ({
  onContinue,
  isVoteWithMetadata,
  onCancel,
  children,
  hideAllBtn = false,
  useBackLabel = false,
  useSubmitLabel = false,
  onSkip,
  continueLabel,
  isContinueDisabled
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
        !hideAllBtn &&
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
                    sx={{ width: isMobile ? "100%" : "96px", whiteSpace: "nowrap" , height:"48px" , fontWeight:"500" }}
                    variant="outlined"
          >
            {useBackLabel ? t('back') :  t("cancel")}
          </Button>
          <Button
            data-testid="confirm-modal-button"
            disabled={isContinueDisabled}
            onClick={isVoteWithMetadata ?   onContinue: onSkip}
            size="large"
            sx={{
              width: isMobile ? "100%" : "130px",
              whiteSpace: "nowrap",
              height: "48px",
              fontWeight: "500",
            }}
            variant="contained"
          >
            {continueLabel
              ? continueLabel
              : useSubmitLabel
              ? t("submit")
              : t("continue")}
          </Button>
        </Box>
      }
    </>
  );
};
