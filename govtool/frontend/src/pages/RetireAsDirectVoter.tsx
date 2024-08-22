import { useCallback, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { Trans } from "react-i18next";
import { CircularProgress, Link } from "@mui/material";

import { Typography } from "@atoms";
import { PATHS } from "@consts";
import { useCardano, useModal, useAppContext } from "@context";
import {
  useGetVoterInfo,
  useScreenDimension,
  useTranslation,
  useWalletErrorModal,
} from "@hooks";
import { CenteredBoxBottomButtons, CenteredBoxPageWrapper } from "@molecules";
import { checkIsWalletConnected, correctAdaFormat, openInNewTab } from "@utils";
import { WrongRouteInfo } from "@organisms";

export const RetireAsDirectVoter = () => {
  const { cExplorerBaseUrl } = useAppContext();
  const navigate = useNavigate();
  const [isLoading, setIsLoading] = useState(false);
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const { voter } = useGetVoterInfo();
  const openWalletErrorModal = useWalletErrorModal();
  const { buildSignSubmitConwayCertTx, buildDRepRetirementCert } = useCardano();
  const { openModal, closeModal } = useModal();

  const onRetire = useCallback(async () => {
    try {
      setIsLoading(true);
      if (!voter?.deposit) {
        throw new Error(t("errors.appCannotGetDeposit"));
      }
      const certBuilder = await buildDRepRetirementCert(
        voter?.deposit?.toString(),
      );
      const result = await buildSignSubmitConwayCertTx({
        certBuilder,
        type: "retireAsDirectVoter",
        voter,
      });
      if (result) {
        openModal({
          type: "statusModal",
          state: {
            status: "success",
            title: t("modals.retirement.title"),
            message: t("modals.retirement.message"),
            link: `${cExplorerBaseUrl}/tx/${result}`,
            buttonText: t("modals.common.goToDashboard"),
            dataTestId: "retirement-transaction-submitted-modal",
            onSubmit: () => {
              navigate(PATHS.dashboard);
              closeModal();
            },
          },
        });
      }
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } catch (error: any) {
      openWalletErrorModal({
        error,
        buttonText: t("modals.common.goToDashboard"),
        onSumbit: () => navigate(PATHS.dashboard),
        dataTestId: "retirement-transaction-error-modal",
      });
    } finally {
      setIsLoading(false);
    }
  }, [
    buildDRepRetirementCert,
    buildSignSubmitConwayCertTx,
    openModal,
    voter?.deposit,
  ]);

  useEffect(() => {
    if (!checkIsWalletConnected()) {
      navigate(PATHS.home);
    }
  }, []);

  const pageTitle = t("directVoter.retireDirectVoter");

  if (!voter)
    return (
      <CenteredBoxPageWrapper pageTitle={pageTitle} showVotingPower hideBox>
        <CircularProgress />
      </CenteredBoxPageWrapper>
    );

  if (!voter.isRegisteredAsSoleVoter)
    return (
      <CenteredBoxPageWrapper pageTitle={pageTitle} showVotingPower>
        <WrongRouteInfo
          title={t("directVoter.notDirectVoter.title")}
          description={t("directVoter.notDirectVoter.description")}
        />
      </CenteredBoxPageWrapper>
    );

  return (
    <CenteredBoxPageWrapper pageTitle={pageTitle} showVotingPower>
      <Typography sx={{ mt: 1, textAlign: "center" }} variant="headline4">
        {t("directVoter.retirementHeading")}
      </Typography>
      <Typography
        fontWeight={400}
        sx={{
          mb: 7,
          mt: isMobile ? 4 : 10,
          textAlign: "center",
          whiteSpace: "pre-line",
        }}
        variant="body1"
      >
        <Trans
          i18nKey="directVoter.retirementDescription"
          values={{ deposit: correctAdaFormat(voter?.deposit) }}
          components={[
            <Link
              onClick={() => openInNewTab("https://sancho.network/")}
              sx={{ cursor: "pointer", textDecoration: "none" }}
              key="0"
            />,
          ]}
        />
      </Typography>
      <CenteredBoxBottomButtons
        onActionButton={onRetire}
        isLoadingActionButton={isLoading}
        backButtonText={t("cancel")}
      />
    </CenteredBoxPageWrapper>
  );
};
