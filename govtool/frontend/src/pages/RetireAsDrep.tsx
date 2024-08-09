import { useEffect } from "react";
import { useNavigate } from "react-router-dom";

import { PATHS } from "@consts";
import { WhatRetirementMeans, WrongRouteInfo } from "@organisms";
import { useGetVoterInfo, useTranslation } from "@hooks";
import { CenteredBoxPageWrapper } from "@molecules";
import { checkIsWalletConnected } from "@utils";
import { CircularProgress } from "@mui/material";

export const RetireAsDrep = () => {
  const navigate = useNavigate();
  const { t } = useTranslation();
  const { voter } = useGetVoterInfo();

  useEffect(() => {
    if (!checkIsWalletConnected()) {
      navigate(PATHS.home);
    }
  }, []);

  const onClickBackToDashboard = () => navigate(PATHS.dashboard);

  const pageTitle = t("retirement.retireAsDrep");

  if (!voter)
    return (
      <CenteredBoxPageWrapper pageTitle={pageTitle} hideBox>
        <CircularProgress />
      </CenteredBoxPageWrapper>
    );

  return (
    <CenteredBoxPageWrapper pageTitle={pageTitle}>
      {!voter.isRegisteredAsDRep ? (
        <WrongRouteInfo
          title={t("retirement.notADRep.title")}
          description={t("retirement.notADRep.description")}
        />
      ) : (
        <WhatRetirementMeans onClickCancel={onClickBackToDashboard} />
      )}
    </CenteredBoxPageWrapper>
  );
};
