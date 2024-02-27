import { useEffect } from "react";
import { useNavigate } from "react-router-dom";

import { PATHS } from "@consts";
import { RegisterAsSoleVoterBox } from "@organisms";
import { useTranslation } from "@hooks";
import { CenteredBoxPageWrapper } from "@molecules";
import { checkIsWalletConnected } from "@/utils";

export const RegisterAsSoleVoter = () => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  useEffect(() => {
    if (checkIsWalletConnected()) {
      navigate(PATHS.home);
    }
  }, []);

  return (
    <CenteredBoxPageWrapper
      pageTitle={t("soleVoter.becomeSoleVoter")}
      backButtonText={t("backToDashboard")}
      backButtonPath={PATHS.dashboard}
      isVotingPowerHidden
    >
      <RegisterAsSoleVoterBox />
    </CenteredBoxPageWrapper>
  );
};
