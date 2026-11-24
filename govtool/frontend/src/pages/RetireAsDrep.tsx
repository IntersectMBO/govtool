import { useEffect, useRef, useState } from "react";
import { useNavigate } from "react-router";
import { useTranslation } from "react-i18next";
import { CircularProgress } from "@mui/material";

import { PATHS } from "@/consts/paths";
import { CenteredBoxPageWrapper } from "@/components/molecules/CenteredBoxPageWrapper";
import { WrongRouteInfo } from "@/components/organisms/WrongRouteInfo";
import {
  RetirementProgressDialog,
} from "@/components/organisms/RegisterAsDRepSteps/RetirementProgressDialog";
import {
  WhatRetirementMeans,
  type RetirementSubmission,
} from "@/components/organisms/RegisterAsDRepSteps/WhatRetirementMeans";
import { useCardano } from "@/context/wallet";
import { useGetVoterInfo } from "@/hooks/queries/useGetVoterInfoQuery";
import { checkIsWalletConnected } from "@/utils/checkIsWalletConnected";

export const RetireAsDrep = () => {
  const navigate = useNavigate();
  const { t } = useTranslation();
  const { voter } = useGetVoterInfo();
  const { pendingTransaction } = useCardano();
  const [submission, setSubmission] = useState<RetirementSubmission | null>(null);

  useEffect(() => {
    if (!checkIsWalletConnected()) {
      navigate(PATHS.home);
    }
  }, []);

  const onClickBackToDashboard = () => navigate(PATHS.dashboard);

  // The progress lives here, not in the form: the pending hash is part of the
  // voter query's key, so the voter reloads (and the form unmounts) as soon
  // as the wallet submits. A reload while the retirement is pending shows it
  // too.
  const pendingHash = pendingTransaction?.retireAsDrep?.transactionHash;
  const txHash = submission?.txHash ?? pendingHash;
  const sawPending = useRef(false);
  useEffect(() => {
    if (pendingHash) sawPending.current = true;
    const isRetired = !!submission && !!voter && !voter.isRegisteredAsDRep;
    const isSettled = sawPending.current && !pendingHash;
    if (isRetired || isSettled) navigate(PATHS.dashboard);
  }, [pendingHash, submission, voter]);

  const pageTitle = t("retirement.retireAsDrep");

  if (txHash) {
    return (
      <CenteredBoxPageWrapper pageTitle={pageTitle} hideBox={!voter?.isRegisteredAsDRep}>
        {voter?.isRegisteredAsDRep && (
          <WhatRetirementMeans onClickCancel={onClickBackToDashboard} onSubmitted={setSubmission} />
        )}
        <RetirementProgressDialog
          deposit={submission?.deposit}
          name={submission?.name}
          onClose={onClickBackToDashboard}
          txHash={txHash}
        />
      </CenteredBoxPageWrapper>
    );
  }

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
        <WhatRetirementMeans onClickCancel={onClickBackToDashboard} onSubmitted={setSubmission} />
      )}
    </CenteredBoxPageWrapper>
  );
};
