import { useCallback } from "react";
import { useNavigate } from "react-router-dom";

import { IMAGES, PATHS } from "@consts";
import { useTranslation } from "@hooks";
import { DashboardActionCard } from "@molecules";
import { openInNewTab } from "@utils";
import { PendingTransaction } from "@/context/pendingTransaction";

type ProposeGovActionDashboardCardProps = {
  pendingTransaction: PendingTransaction;
};

export const ProposeGovActionDashboardCard = ({
  pendingTransaction,
}: ProposeGovActionDashboardCardProps) => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  const onClickGovernanceActionCardActionButton = useCallback(() => {
    if (!pendingTransaction.createGovAction) {
      navigate(PATHS.dashboardGovernanceActions);
      return;
    }
    navigate(PATHS.createGovernanceAction);
  }, [pendingTransaction, navigate]);

  return (
    <DashboardActionCard
      dataTestidFirstButton="propose-governance-actions-button"
      description={t("dashboard.proposeGovernanceAction.description")}
      firstButtonAction={onClickGovernanceActionCardActionButton}
      firstButtonLabel={t(
        `dashboard.proposeGovernanceAction.${
          pendingTransaction.createGovAction ? "view" : "propose"
        }`
      )}
      inProgress={!!pendingTransaction.createGovAction}
      secondButtonLabel={t("learnMore")}
      secondButtonAction={() =>
        openInNewTab(
          "https://docs.sanchogov.tools/faqs/what-is-a-governance-action"
        )}
      secondButtonVariant="outlined"
      imageURL={IMAGES.proposeGovActionImage}
      title={t("dashboard.proposeGovernanceAction.title")}
    />
  );
};
