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
      dataTestidFirstButton="view-governance-actions-button"
      description={t("dashboard.govActions.description")}
      firstButtonAction={() => navigate(PATHS.dashboardGovernanceActions)}
      firstButtonLabel={t(
        `dashboard.govActions.${
          voter?.isRegisteredAsDRep ? "reviewAndVote" : "view"
        }`
      )}
      imageURL={IMAGES.govActionListImage}
      title={t("dashboard.govActions.title")}
    />
  );
};
