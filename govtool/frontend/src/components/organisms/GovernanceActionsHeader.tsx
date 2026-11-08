import { useNavigate } from "react-router";
import { Box } from "@mui/material";
import { PackagePlus } from "lucide-react";

import { Button } from "@atoms";
import { PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import { useTranslation } from "@hooks";
import { PageTitle } from "@molecules";

import { GovernanceActionsTabs } from "./GovernanceActionsTabs";

/**
 * Title, description, Create Governance Action and the Proposed / Live Voting
 * / Outcomes tabs, shared by the governance action list pages.
 */
export const GovernanceActionsHeader = () => {
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { isEnabled, stakeKey } = useCardano();
  const { openModal } = useModal();
  const isConnected = Boolean(isEnabled && stakeKey);

  const onCreate = () => {
    if (isConnected) navigate(PATHS.createGovernanceAction);
    else openModal({ type: "chooseWallet" });
  };

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 3 }}>
      <PageTitle
        action={
          <Button
            dataTestId="create-governance-action-button"
            onClick={onCreate}
            size="large"
            startIcon={<PackagePlus aria-hidden size={18} />}
          >
            {t("govActions.createAction")}
          </Button>
        }
        description={t("govActions.pageDescription")}
        title={t("govActions.pageTitle")}
      />
      <GovernanceActionsTabs />
    </Box>
  );
};
