import { useCallback, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useTranslation } from "react-i18next";

import { PATHS } from "@consts";
import { RegisterAsDirectVoterBoxContent } from "@organisms";
import { CenteredBoxBottomButtons } from "@molecules";
import { useCardano, useModal } from "@context";
import { useGetVoterInfo, useWalletErrorModal } from "@hooks";

export const RegisterAsDirectVoterBox = () => {
  const [isLoading, setIsLoading] = useState<boolean>(false);

  const { buildSignSubmitConwayCertTx, buildDRepRegCert, buildDRepUpdateCert } =
    useCardano();
  const navigate = useNavigate();
  const { openModal, closeModal } = useModal();
  const { t } = useTranslation();
  const { voter } = useGetVoterInfo();
  const openWalletErrorModal = useWalletErrorModal();

  const onRegister = useCallback(async () => {
    setIsLoading(true);

    try {
      const certBuilder = voter?.isRegisteredAsDRep
        ? await buildDRepUpdateCert()
        : await buildDRepRegCert();
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
            link: `https://sancho.cexplorer.io/tx/${result}`,
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
        error,
        buttonText: t("modals.common.goToDashboard"),
        onSumbit: () => navigate(PATHS.dashboard),
        dataTestId: "registration-transaction-error-modal",
      });
    } finally {
      setIsLoading(false);
    }
  }, [buildSignSubmitConwayCertTx, buildDRepRegCert, openModal]);

  return (
    <>
      <RegisterAsDirectVoterBoxContent />
      <CenteredBoxBottomButtons
        onBackButton={() => navigate(PATHS.dashboard)}
        onActionButton={onRegister}
        isLoading={isLoading}
      />
    </>
  );
};
