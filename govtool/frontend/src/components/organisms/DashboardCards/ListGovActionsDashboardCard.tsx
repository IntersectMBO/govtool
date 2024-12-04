import { useNavigate } from "react-router-dom";

import { IMAGES, PATHS } from "@consts";
import { useTranslation } from "@hooks";
import { DashboardActionCard } from "@molecules";
import { openInNewTab } from "@utils";
import { LINKS } from "@/consts/links";

export const ListGovActionsDashboardCards = () => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  return (
    <DashboardActionCard
      buttons={[
        {
          children: t("dashboard.cards.govActions.title"),
          dataTestId: "view-governance-actions-button",
          onClick: () => navigate(PATHS.dashboardGovernanceActions),
          variant: "contained",
        },
        {
          children: t("learnMore"),
          dataTestId: "list-gov-actions-learn-more-button",
          onClick: () => openInNewTab(LINKS.VIEW_GOVERNANCE_ACTIONS),
          variant: "outlined",
        },
      ]}
      description={t("dashboard.cards.govActions.description")}
      imageURL={IMAGES.govActionListImage}
      title={t("dashboard.cards.govActions.title")}
      type="list-gov-actions"
    />
  );
};
