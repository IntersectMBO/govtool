import { useNavigate } from "react-router-dom";

import { IMAGES, PATHS } from "@consts";
import { useTranslation } from "@hooks";
import { DashboardActionCard } from "@molecules";
import { openInNewTab } from "@utils";
import { PendingTransaction } from "@/context/pendingTransaction";

type ProposeGovActionDashboardCardProps = {
  createGovActionTx: PendingTransaction["createGovAction"];
};

export const ProposeGovActionDashboardCard = ({
  createGovActionTx,
}: ProposeGovActionDashboardCardProps) => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  return (
    <DashboardActionCard
      buttons={[
        ...(createGovActionTx
          // transaction in progress
          ? [
              {
                children: t("dashboard.proposeGovernanceAction.view"),
                dataTestId: "propose-governance-actions-button",
                onClick: () => navigate(PATHS.dashboardGovernanceActions),
                variant: "contained",
              } as const,
          ]
          // default
          : [
              {
                children: t("dashboard.proposeGovernanceAction.propose"),
                dataTestId: "propose-governance-actions-button",
                onClick: () => navigate(PATHS.createGovernanceAction),
                variant: "contained",
              } as const,
          ]),
        // common
        {
          children: t("learnMore"),
          dataTestId: "learn-more-button",
          onClick: () =>
            openInNewTab(
              "https://docs.sanchogov.tools/faqs/what-is-a-governance-action"
            ),
        },
      ]}
      description={t("dashboard.proposeGovernanceAction.description")}
      imageURL={IMAGES.proposeGovActionImage}
      state={createGovActionTx ? "inProgress" : "default"}
      title={t("dashboard.proposeGovernanceAction.title")}
    />
  );
};
