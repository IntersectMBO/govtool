import { useCallback, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useTranslation } from "react-i18next";

import { PATHS } from "@consts";
import { CenteredBoxBottomButtons } from "@molecules";
import { useCardano, useModal } from "@context";
import { RetireAsSoleVoterBoxContent } from "@organisms";
import { useGetVoterInfo } from "@/hooks";

export const RetireAsSoleVoterBox = () => {
  const [isLoading, setIsLoading] = useState<boolean>(false);

  const navigate = useNavigate();
  const {
    buildDRepRetirementCert,
    buildSignSubmitConwayCertTx,
    isPendingTransaction,
  } = useCardano();
  const { openModal, closeModal } = useModal();
  const { t } = useTranslation();
  const { voter } = useGetVoterInfo();

  const onRetire = useCallback(async () => {
    try {
      setIsLoading(true);
      const isPendingTx = isPendingTransaction();
      if (isPendingTx) return;
      if (!voter?.deposit) {
        throw new Error(t("errors.appCannotGetDeposit"));
      }
      const certBuilder = await buildDRepRetirementCert(
        voter?.deposit?.toString(),
      );
      const result = await buildSignSubmitConwayCertTx({
        certBuilder,
        type: "retireAsSoleVoter",
        voterDeposit: voter?.deposit?.toString(),
      });
      if (result) {
        openModal({
          type: "statusModal",
          state: {
            status: "success",
            title: t("modals.retirement.title"),
            message: t("modals.retirement.message"),
            link: "https://sancho.cexplorer.io/tx/" + result,
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
      const errorMessage = error.info ? error.info : error;

      openModal({
        type: "statusModal",
        state: {
          status: "warning",
          message: errorMessage,
          buttonText: t("modals.common.goToDashboard"),
          title: t("modals.common.oops"),
          dataTestId: "retirement-transaction-error-modal",
          onSubmit: () => {
            navigate(PATHS.dashboard);
            closeModal();
          },
        },
      });
    } finally {
      setIsLoading(false);
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
      <RetireAsSoleVoterBoxContent />
      <CenteredBoxBottomButtons
        onBackButton={() => navigate(PATHS.dashboard)}
        onActionButton={onRetire}
        isLoading={isLoading}
      />
    </>
  );
};
