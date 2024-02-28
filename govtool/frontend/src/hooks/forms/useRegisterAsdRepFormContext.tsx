import { useCallback, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useFormContext } from "react-hook-form";

import { PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import { UrlAndHashFormValues, useTranslation } from "@hooks";

export const useRegisterAsdRepFormContext = () => {
  const {
    buildSignSubmitConwayCertTx,
    buildDRepRegCert,
    buildDRepUpdateCert,
    voter,
  } = useCardano();
  const { openModal, closeModal } = useModal();
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const navigate = useNavigate();
  const { t } = useTranslation();

  const {
    control,
    handleSubmit,
    formState: { errors, isValid },
    watch,
  } = useFormContext<UrlAndHashFormValues>();

  const onSubmit = useCallback(
    async (values: UrlAndHashFormValues) => {
      const { url, hash } = values;

      // Temporary solution. To modify later.
      const urlSubmitValue = !url
        ? "https://raw.githubusercontent.com/Thomas-Upfield/test-metadata/main/placeholder.json"
        : url;
      const hashSubmitValue = !hash
        ? "654e483feefc4d208ea02637a981a2046e17c73c09583e9dd0c84c25dab42749"
        : hash;
      setIsLoading(true);

      try {
        const certBuilder = voter?.isRegisteredAsSoleVoter
          ? await buildDRepUpdateCert(urlSubmitValue, hashSubmitValue)
          : await buildDRepRegCert(urlSubmitValue, hashSubmitValue);
        const result = await buildSignSubmitConwayCertTx({
          certBuilder,
          type: "registration",
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
    },
    [buildSignSubmitConwayCertTx, buildDRepRegCert, openModal]
  );

  return {
    isRegistrationAsDRepLoading: isLoading,
    control,
    errors,
    isValid,
    watch,
    submitForm: handleSubmit(onSubmit),
  };
};
