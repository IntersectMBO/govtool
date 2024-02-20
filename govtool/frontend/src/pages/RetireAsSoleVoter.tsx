import { useEffect } from "react";

import { PATHS } from "@consts";
import { RetireAsSoleVoterBox } from "@organisms";
import { useTranslation } from "@hooks";
import { useNavigate } from "react-router-dom";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@/utils/localStorage";
import { CenteredBoxPageWrapper } from "@molecules";

export const RetireAsSoleVoter = () => {
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
      pageTitle={t("soleVoter.retireSoleVoter")}
      backButtonText={t("backToDashboard")}
      backButtonPath={PATHS.dashboard}
    >
      <RetireAsSoleVoterBox />
    </CenteredBoxPageWrapper>
  );
};
