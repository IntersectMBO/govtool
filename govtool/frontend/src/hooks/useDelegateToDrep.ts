import { useCallback, useState } from "react";
import { useTranslation } from "@hooks";
import { useCardano, useSnackbar } from "@/context";

export const useDelegateTodRep = () => {
  const {
    buildSignSubmitConwayCertTx,
    buildVoteDelegationCert,
  } = useCardano();
  const { t } = useTranslation();
  const { addSuccessAlert, addErrorAlert } = useSnackbar();

  const [isDelegating, setIsDelegating] = useState(false);

  const delegate = useCallback(async (dRepId: string | undefined) => {
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
      addErrorAlert(t("alerts.delegate.failed"));
    } finally {
      setIsDelegating(false);
    }
  }, [
    addErrorAlert,
    addSuccessAlert,
    buildSignSubmitConwayCertTx,
    buildVoteDelegationCert,
    t,
  ]);

  return {
    delegate,
    isDelegating,
  };
};
