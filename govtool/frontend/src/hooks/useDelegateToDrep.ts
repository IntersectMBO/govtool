import { useCallback, useState } from "react";

import { useCardano, useSnackbar } from "@context";
import { useGetVoterInfo, useTranslation, useWalletErrorModal } from "@hooks";
import { CertificatesBuilder } from "@emurgo/cardano-serialization-lib-asmjs";

export const useDelegateTodRep = () => {
  const {
    buildSignSubmitConwayCertTx,
    buildVoteDelegationCert,
    buildDRepRetirementCert,
    buildStakeKeyRegCert,
    registeredStakeKeysListState,
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
        const certBuilder = CertificatesBuilder.new();

        if (voter?.isRegisteredAsSoleVoter) {
          const retirementCert = await buildDRepRetirementCert(
            voter?.deposit?.toString(),
          );
          certBuilder.add(retirementCert);
        }

        if (!registeredStakeKeysListState.length) {
          const stakeKeyRegCert = await buildStakeKeyRegCert();
          certBuilder.add(stakeKeyRegCert);
        }

        const voteDelegationCert = await buildVoteDelegationCert(dRepId);
        certBuilder.add(voteDelegationCert);

        await buildSignSubmitConwayCertTx({
          certBuilder,
          type: "delegate",
          resourceId: dRepId,
          voter,
        });
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } catch (error: any) {
        openWalletErrorModal({
          error: error?.message ? error.message : JSON.stringify(error),
          dataTestId: "delegate-transaction-error-modal",
        });
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
