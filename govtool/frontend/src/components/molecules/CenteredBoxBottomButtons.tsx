import { useNavigate } from "react-router";
import { Box } from "@mui/material";

import { Button } from "@atoms";
import { PATHS } from "@consts";
import { useTranslation } from "@hooks";
import { ds } from "@/consts/designTokens";

interface Props {
  actionButtonText?: string;
  actionButtonDataTestId?: string;
  onActionButton: () => void;
  isLoadingActionButton?: boolean;
  disableActionButton?: boolean;
  backButtonText?: string;
  onBackButton?: () => void;
}

export const CenteredBoxBottomButtons = ({
  actionButtonText,
  actionButtonDataTestId,
  backButtonText,
  isLoadingActionButton,
  disableActionButton,
  onBackButton,
  onActionButton,
}: Props) => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  const navigateToDashboard = () => navigate(PATHS.dashboard);

  return (
    <Box
      sx={{
        borderTop: `1px solid ${ds.gray["100"]}`,
        display: "flex",
        flexDirection: { xxs: "column-reverse", sm: "row" },
        gap: 1.5,
        justifyContent: "space-between",
        mt: 4,
        pt: 3,
        "& > *": { width: { xxs: "100%", sm: "auto" } },
      }}
    >
      <Button
        data-testid="back-button"
        onClick={onBackButton ?? navigateToDashboard}
        size="large"
        variant="outlined"
      >
        {backButtonText ?? t("back")}
      </Button>
      <Button
        data-testid={actionButtonDataTestId ?? "continue-button"}
        disabled={disableActionButton || isLoadingActionButton}
        isLoading={isLoadingActionButton}
        onClick={onActionButton}
        size="large"
        variant="contained"
      >
        {actionButtonText ?? t("continue")}
      </Button>
    </Box>
  );
};
