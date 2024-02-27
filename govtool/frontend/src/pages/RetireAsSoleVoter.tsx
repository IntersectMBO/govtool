import { useEffect } from "react";

import { PATHS } from "@consts";
import { RetireAsSoleVoterBox } from "@organisms";
import { useTranslation } from "@hooks";
import { useNavigate } from "react-router-dom";
import { CenteredBoxPageWrapper } from "@molecules";
import { checkIsWalletConnected } from "@/utils";

export const RetireAsSoleVoter = () => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  useEffect(() => {
    if (checkIsWalletConnected()) {
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
