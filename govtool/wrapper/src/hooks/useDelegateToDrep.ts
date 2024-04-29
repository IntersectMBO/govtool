import { useCallback, useState } from "react";
import * as Sentry from "@sentry/react";
import { useTranslation, useWalletErrorModal } from "@hooks";
import { useCardano, useSnackbar } from "@/context";

export const useDelegateTodRep = () => {
  const { buildSignSubmitConwayCertTx, buildVoteDelegationCert } = useCardano();
  const { t } = useTranslation();
  const { addSuccessAlert, addErrorAlert } = useSnackbar();
  const openWalletErrorModal = useWalletErrorModal();

  const [isDelegating, setIsDelegating] = useState(false);

  const delegate = useCallback(
    async (dRepId: string | undefined) => {
      if (!dRepId) return;
      setIsDelegating(true);
      try {
        const certBuilder = await buildVoteDelegationCert(dRepId);
        const result = await buildSignSubmitConwayCertTx({
          certBuilder,
          type: "delegate",
          resourceId: dRepId,
        });
        if (result) {
          addSuccessAlert(t("alerts.delegate.success"));
        }
      } catch (error) {
        openWalletErrorModal({
          error,
          dataTestId: "delegate-transaction-error-modal",
        });
        Sentry.captureException(error);
      } finally {
        setIsDelegating(false);
      }
    },
    [
      addErrorAlert,
      addSuccessAlert,
      buildSignSubmitConwayCertTx,
      buildVoteDelegationCert,
      t,
    ],
  );

  return {
    delegate,
    isDelegating,
  };
};
