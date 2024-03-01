import { useCallback, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useTranslation } from "react-i18next";

import { PATHS } from "@consts";
import { RegisterAsSoleVoterBoxContent } from "@organisms";
import { CenteredBoxBottomButtons } from "@molecules";
import { useCardano, useModal } from "@context";

export const RegisterAsSoleVoterBox = () => {
  const [isLoading, setIsLoading] = useState<boolean>(false);

  const {
    buildSignSubmitConwayCertTx,
    buildDRepRegCert,
    buildDRepUpdateCert,
    voter,
  } = useCardano();
  const navigate = useNavigate();
  const { openModal, closeModal } = useModal();
  const { t } = useTranslation();

  const onRegister = useCallback(async () => {
    setIsLoading(true);

    try {
      const certBuilder = voter?.isRegisteredAsDRep
        ? await buildDRepUpdateCert()
        : await buildDRepRegCert();
      const result = await buildSignSubmitConwayCertTx({
        certBuilder,
        type: "soleVoterRegistration",
        registrationType: "registration",
      });
      if (result)
        openModal({
          type: "statusModal",
          state: {
            status: "success",
            title: t("modals.registration.title"),
            message: t("modals.registration.message"),
            link: "https://adanordic.com/latest_transactions",
            buttonText: t("modals.common.goToDashboard"),
            onSubmit: () => {
              navigate(PATHS.dashboard);
              closeModal();
            },
            dataTestId: "registration-transaction-submitted-modal",
          },
        });
    } catch (e: any) {
      const errorMessage = e.info ? e.info : e;

      openModal({
        type: "statusModal",
        state: {
          status: "warning",
          title: t("modals.common.oops"),
          message: errorMessage,
          buttonText: t("modals.common.goToDashboard"),
          onSubmit: () => {
            navigate(PATHS.dashboard);
            closeModal();
          },
          dataTestId: "registration-transaction-error-modal",
        },
      });
    } finally {
      setIsLoading(false);
    }
  }, [buildSignSubmitConwayCertTx, buildDRepRegCert, openModal]);

  return (
    <>
      <RegisterAsSoleVoterBoxContent />
      <CenteredBoxBottomButtons
        onBackButton={() => navigate(PATHS.dashboard)}
        onActionButton={onRegister}
        isLoading={isLoading}
      />
    </>
  );
};
