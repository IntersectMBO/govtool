import { useCallback, useState } from "react";

import { Typography } from "@atoms";
import { useCardano, useModal } from "@context";
import { useGetVoterInfo, useScreenDimension, useTranslation } from "@hooks";

import { BgCard } from "..";

export const WhatRetirementMeans = ({
  onClickCancel,
}: {
  onClickCancel: () => void;
}) => {
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

  const onSubmit = () => {
    onClickCancel();
    closeModal();
  };

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
        voterDeposit: voter?.deposit.toString(),
      });
      if (result) {
        openModal({
          type: "statusModal",
          state: {
            buttonText: t("modals.common.goToDashboard"),
            dataTestId: "retirement-transaction-submitted-modal",
            link: "https://sancho.cexplorer.io/tx/" + result,
            message: t("modals.retirement.message"),
            onSubmit,
            status: "success",
            title: t("modals.retirement.title"),
          },
        });
      }
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } catch (error: any) {
      const errorMessage = error.info ? error.info : error;

      openModal({
        type: "statusModal",
        state: {
          buttonText: t("modals.common.goToDashboard"),
          dataTestId: "retirement-transaction-error-modal",
          message: errorMessage,
          onSubmit,
          status: "warning",
          title: t("modals.common.oops"),
        },
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
    <BgCard
      actionButtonLabel={t("retirement.continue")}
      backButtonLabel={t("cancel")}
      isLoadingActionButton={isRetirementLoading}
      onClickActionButton={retireAsDrep}
      onClickBackButton={onClickCancel}
      sx={{ pb: isMobile ? undefined : 5, pt: isMobile ? 4 : 8 }}
    >
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
    </BgCard>
  );
};
