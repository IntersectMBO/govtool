import { ReactNode } from "react";
import { useNavigate } from "react-router";
import { Box } from "@mui/material";
import { PackagePlus } from "lucide-react";

import { Button } from "@atoms";
import { PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import { useTranslation } from "@hooks";
import { PageTitle } from "@molecules";

import { GovernanceActionsStats } from "./GovernanceActionsStats";
import { GovernanceActionsTabs } from "./GovernanceActionsTabs";

type Props = {
  /** Replaces Create Governance Action (the Proposed tab proposes instead). */
  action?: ReactNode;
  /** A notice under the title, such as what proposing still needs. */
  notice?: ReactNode;
};

/**
 * Title, description, the page's main action, the four governance action
 * figures and the Proposed / Live Voting / Outcomes tabs, shared by the
 * governance action list pages.
 */
export const GovernanceActionsHeader = ({ action, notice }: Props) => {
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
      <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
        <PageTitle
          action={
            action ?? (
              <Button
                dataTestId="create-governance-action-button"
                onClick={onCreate}
                size="large"
                startIcon={<PackagePlus aria-hidden size={18} />}
              >
                {t("govActions.createAction")}
              </Button>
            )
          }
          description={t("govActions.pageDescription")}
          title={t("govActions.pageTitle")}
        />
        {notice}
      </Box>
      <GovernanceActionsStats />
      <GovernanceActionsTabs />
    </Box>
  );
};
