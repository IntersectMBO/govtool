import { useCallback, useEffect, useState } from "react";
import * as Sentry from "@sentry/react";

import { Typography } from "@atoms";
import { useCardano, useModal, useAppContext } from "@context";
import {
  useGetVoterInfo,
  useScreenDimension,
  useTranslation,
  useWalletErrorModal,
} from "@hooks";
import { CenteredBoxBottomButtons } from "@molecules";

export const WhatRetirementMeans = ({
  onClickCancel,
}: {
  onClickCancel: () => void;
}) => {
  const { cExplorerBaseUrl } = useAppContext();
  const {
    isPendingTransaction,
    buildDRepRetirementCert,
    buildSignSubmitConwayCertTx,
  } = useCardano();
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const { closeModal, openModal } = useModal();
  const [isRetirementLoading, setIsRetirementLoading] =
    useState<boolean>(false);
  const { voter } = useGetVoterInfo();
  const openWalletErrorModal = useWalletErrorModal();

  const onSubmit = () => {
    onClickCancel();
    closeModal();
  };

  useEffect(() => {
    Sentry.setTag("component_name", "WhatRetirementMeans");
  }, []);

  const retireAsDrep = useCallback(async () => {
    try {
      setIsRetirementLoading(true);
      const isPendingTx = isPendingTransaction();
      if (isPendingTx) return;
      if (!voter?.deposit) throw new Error(t("errors.appCannotGetDeposit"));

      const certBuilder = await buildDRepRetirementCert(
        voter?.deposit.toString(),
      );
      const result = await buildSignSubmitConwayCertTx({
        certBuilder,
        type: "retireAsDrep",
        voter,
      });
      if (result) {
        openModal({
          type: "statusModal",
          state: {
            buttonText: t("modals.common.goToDashboard"),
            dataTestId: "retirement-transaction-submitted-modal",
            link: `${cExplorerBaseUrl}/tx/${result}`,
            message: t("modals.retirement.message"),
            onSubmit,
            status: "success",
            title: t("modals.retirement.title"),
          },
        });
      }
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } catch (error: any) {
      Sentry.captureException(error);
      openWalletErrorModal({
        error: error?.message ? error.message : JSON.stringify(error),
        onSumbit: onClickCancel,
        buttonText: t("modals.common.goToDashboard"),
        dataTestId: "retirement-transaction-error-modal",
      });
    } finally {
      setIsRetirementLoading(false);
    }
  }, [
    buildDRepRetirementCert,
    buildSignSubmitConwayCertTx,
    isPendingTransaction,
    openModal,
    voter?.deposit,
  ]);

  return (
    <>
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("retirement.whatRetirementMeansTitle")}
      </Typography>
      <Typography
        fontWeight={400}
        sx={{
          pb: isMobile ? 4 : 6,
          pt: 4,
          textAlign: "center",
          whiteSpace: "pre-line",
        }}
        variant="body1"
      >
        {t("retirement.whatRetirementMeansDescription")}
      </Typography>
      <CenteredBoxBottomButtons
        actionButtonText={t("retirement.continue")}
        actionButtonDataTestId="continue-retirement-button"
        backButtonText={t("cancel")}
        isLoadingActionButton={isRetirementLoading}
        onActionButton={retireAsDrep}
        onBackButton={onClickCancel}
      />
    </>
  );
};
