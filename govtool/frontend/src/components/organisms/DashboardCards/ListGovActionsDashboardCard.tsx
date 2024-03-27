import { useNavigate } from "react-router-dom";

import { IMAGES, PATHS } from "@consts";
import { useTranslation } from "@hooks";
import { DashboardActionCard } from "@molecules";
import { VoterInfo } from "@/models";

type ListGovActionsDashboardCardsProps = {
  voter: VoterInfo;
};

export const ListGovActionsDashboardCards = ({ voter }: ListGovActionsDashboardCardsProps) => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  return (
    <DashboardActionCard
      buttons={[
        {
          children: t(
            `dashboard.govActions.${
              voter?.isRegisteredAsDRep ? "reviewAndVote" : "view"
            }`
          ),
          dataTestId: "view-governance-actions-button",
          onClick: () => navigate(PATHS.dashboardGovernanceActions),
        },
      ]}
      description={t("dashboard.govActions.description")}
      imageURL={IMAGES.govActionListImage}
      title={t("dashboard.govActions.title")}
    />
  );
};
