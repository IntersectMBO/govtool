import { useNavigate } from "react-router-dom";

import { IMAGES, PATHS } from "@consts";
import { useTranslation } from "@hooks";
import { DashboardActionCard } from "@molecules";
import { openInNewTab } from "@utils";

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
          onClick: () =>
            openInNewTab(
              "https://docs.gov.tools/using-govtool/govtool-functions/governance-actions/view-governance-actions",
            ),
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
