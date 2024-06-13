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
          dataTestId: "learn-more-governance-actions-button",
          onClick: () =>
            openInNewTab(
              "https://docs.sanchogov.tools/how-to-use-the-govtool/using-govtool/governance-actions-view-and-vote",
            ),
          variant: "outlined",
        },
      ]}
      description={t("dashboard.cards.govActions.description")}
      imageURL={IMAGES.govActionListImage}
      title={t("dashboard.cards.govActions.title")}
    />
  );
};
