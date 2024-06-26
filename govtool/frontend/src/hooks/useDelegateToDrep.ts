import { useCallback, useState } from "react";
import * as Sentry from "@sentry/react";

import { useCardano, useSnackbar } from "@context";
import { useGetVoterInfo, useTranslation, useWalletErrorModal } from "@hooks";

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

  const [isDelegating, setIsDelegating] = useState<string | null>(null);

  const delegate = useCallback(
    async (dRepId: string | undefined) => {
      if (!dRepId) return;
      setIsDelegating(dRepId);
      try {
        if (voter?.isRegisteredAsSoleVoter && !voter?.deposit) {
          throw new Error(t("errors.appCannotGetDeposit"));
        }

        const certBuilder = await buildVoteDelegationCert(dRepId);
        if (voter?.isRegisteredAsSoleVoter) {
          const retirementCert = await buildDRepRetirementCert(
            voter?.deposit?.toString(),
          );
          certBuilder.add(retirementCert);
        }
        await buildSignSubmitConwayCertTx({
          certBuilder,
          type: "delegate",
          resourceId: dRepId,
          voter,
        });
      } catch (error) {
        openWalletErrorModal({
          error,
          dataTestId: "delegate-transaction-error-modal",
        });
        Sentry.setTag("hook", "useDelegateTodRep");
        Sentry.captureException(error);
      } finally {
        setIsDelegating(null);
      }
    },
    [
      addErrorAlert,
      addSuccessAlert,
      buildSignSubmitConwayCertTx,
      buildVoteDelegationCert,
      t,
      voter?.deposit,
      voter?.isRegisteredAsSoleVoter,
    ],
  );

  return {
    delegate,
    isDelegating,
  };
};
