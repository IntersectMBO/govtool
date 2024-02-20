import { useEffect } from "react";
import { useNavigate } from "react-router-dom";

import { PATHS } from "@consts";
import { RegisterAsSoleVoterBox } from "@organisms";
import { useTranslation } from "@hooks";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@/utils/localStorage";
import { CenteredBoxPageWrapper } from "@molecules";

export const RegisterAsSoleVoter = () => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  useEffect(() => {
    if (
      !getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`) ||
      !getItemFromLocalStorage(`${WALLET_LS_KEY}_name`)
    ) {
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
