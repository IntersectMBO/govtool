import { useEffect } from "react";
import { useNavigate } from "react-router-dom";

import { PATHS } from "@consts";
import { WhatRetirementMeans } from "@organisms";
import { useTranslation } from "@hooks";
import { CenteredBoxPageWrapper } from "@molecules";
import { checkIsWalletConnected } from "@utils";

export const RetireAsDrep = () => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  useEffect(() => {
    if (!checkIsWalletConnected()) {
      navigate(PATHS.home);
    }
  }, []);

  const onClickBackToDashboard = () => navigate(PATHS.dashboard);

  return (
    <CenteredBoxPageWrapper
      pageTitle={t("retirement.retireAsDrep")}
      onClickBackToDashboard={onClickBackToDashboard}
    >
      <WhatRetirementMeans onClickCancel={onClickBackToDashboard} />
    </CenteredBoxPageWrapper>
  );
};
