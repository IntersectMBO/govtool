import { useCallback, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { Trans } from "react-i18next";
import { CircularProgress, Link } from "@mui/material";

import { Typography } from "@atoms";
import { PATHS } from "@consts";
import { useAppContext, useCardano, useModal } from "@context";
import {
  useGetVoterInfo,
  useScreenDimension,
  useTranslation,
  useWalletErrorModal,
} from "@hooks";
import { CenteredBoxBottomButtons, CenteredBoxPageWrapper } from "@molecules";
import { checkIsWalletConnected, openInNewTab } from "@utils";
import { WrongRouteInfo } from "@organisms";
import { CertificatesBuilder } from "@emurgo/cardano-serialization-lib-asmjs";

export const RegisterAsDirectVoter = () => {
  const { cExplorerBaseUrl } = useAppContext();
  const navigate = useNavigate();
  const [isLoading, setIsLoading] = useState(false);
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const { voter } = useGetVoterInfo();
  const openWalletErrorModal = useWalletErrorModal();
  const {
    buildStakeKeyRegCert,
    buildSignSubmitConwayCertTx,
    buildDRepRegCert,
    buildDRepUpdateCert,
    buildVoteDelegationCert,
    dRepID,
    registeredStakeKeysListState,
  } = useCardano();
  const { openModal, closeModal } = useModal();

  const onRegister = useCallback(async () => {
    setIsLoading(true);

    try {
      const certBuilder = CertificatesBuilder.new();

      const registerCert = voter?.isRegisteredAsDRep
        ? await buildDRepUpdateCert()
        : await buildDRepRegCert();
      certBuilder.add(registerCert);

      if (!registeredStakeKeysListState.length) {
        const stakeKeyRegCert = await buildStakeKeyRegCert();
        certBuilder.add(stakeKeyRegCert);
      }

      const voteDelegationCert = await buildVoteDelegationCert(dRepID);
      certBuilder.add(voteDelegationCert);

      const result = await buildSignSubmitConwayCertTx({
        certBuilder,
        type: "registerAsDirectVoter",
      });
      if (result) {
        openModal({
          type: "statusModal",
          state: {
            status: "success",
            title: t("modals.registration.title"),
            message: t("modals.registration.message"),
            link: `${cExplorerBaseUrl}/tx/${result}`,
            buttonText: t("modals.common.goToDashboard"),
            onSubmit: () => {
              navigate(PATHS.dashboard);
              closeModal();
            },
            dataTestId: "registration-transaction-submitted-modal",
          },
        });
      }
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } catch (error: any) {
      openWalletErrorModal({
        error: error?.message ? error.message : JSON.stringify(error),
        buttonText: t("modals.common.goToDashboard"),
        onSumbit: () => navigate(PATHS.dashboard),
        dataTestId: "registration-transaction-error-modal",
      });
    } finally {
      setIsLoading(false);
    }
  }, [
    buildSignSubmitConwayCertTx,
    buildDRepRegCert,
    dRepID,
    openModal,
    voter?.isRegisteredAsDRep,
  ]);

  useEffect(() => {
    if (!checkIsWalletConnected()) {
      navigate(PATHS.home);
    }
  }, []);

  const pageTitle = t("directVoter.becomeDirectVoter");

  if (!voter)
    return (
      <CenteredBoxPageWrapper pageTitle={pageTitle} hideBox>
        <CircularProgress />
      </CenteredBoxPageWrapper>
    );

  if (voter?.isRegisteredAsSoleVoter)
    return (
      <CenteredBoxPageWrapper pageTitle={pageTitle}>
        <WrongRouteInfo
          title={t(`directVoter.alreadyRegistered.title`)}
          description={t(`directVoter.alreadyRegistered.description`)}
        />
      </CenteredBoxPageWrapper>
    );

  return (
    <CenteredBoxPageWrapper pageTitle={pageTitle}>
      <Typography sx={{ mt: 1, textAlign: "center" }} variant="headline4">
        {t("directVoter.registerHeading")}
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
          i18nKey="directVoter.registerDescription"
          components={[
            <Link
              onClick={() =>
                openInNewTab(
                  "https://docs.gov.tools/cardano-govtool/faqs/direct-voter-vs-drep",
                )
              }
              sx={{ cursor: "pointer" }}
              key="0"
            />,
          ]}
        />
      </Typography>
      <CenteredBoxBottomButtons
        onActionButton={onRegister}
        isLoadingActionButton={isLoading}
        backButtonText={t("cancel")}
      />
    </CenteredBoxPageWrapper>
  );
};
