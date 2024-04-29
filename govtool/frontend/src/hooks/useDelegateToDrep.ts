import { useCallback, useState } from "react";
import * as Sentry from "@sentry/react";
import { useGetVoterInfo, useTranslation, useWalletErrorModal } from "@hooks";
import { useCardano, useSnackbar } from "@/context";

export const useDelegateTodRep = () => {
  const {
    buildSignSubmitConwayCertTx,
    buildVoteDelegationCert,
    buildDRepRetirementCert,
  } = useCardano();
  const { t } = useTranslation();
  const { addSuccessAlert, addErrorAlert } = useSnackbar();
  const openWalletErrorModal = useWalletErrorModal();
  const { voter } = useGetVoterInfo();

  const [isDelegating, setIsDelegating] = useState(false);

  const delegate = useCallback(
    async (dRepId: string | undefined) => {
      if (!dRepId) return;
      setIsDelegating(true);
      try {
        if (!voter?.deposit) {
          throw new Error(t("errors.appCannotGetDeposit"));
        }
        const retirementCert = await buildDRepRetirementCert(
          voter?.deposit?.toString(),
        );
        const certBuilder = await buildVoteDelegationCert(dRepId);
        certBuilder.add(retirementCert);
        const result = await buildSignSubmitConwayCertTx({
          certBuilder,
          type: "delegate",
          resourceId: dRepId,
          voterDeposit: voter?.deposit?.toString(),
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
