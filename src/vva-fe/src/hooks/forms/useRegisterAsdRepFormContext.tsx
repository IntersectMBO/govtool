import { useCallback, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useFormContext, useWatch } from "react-hook-form";

import { PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import { UrlAndHashFormValues } from "@hooks";

export const useRegisterAsdRepFormContext = () => {
  const { buildSignSubmitConwayCertTx, buildDRepRegCert } = useCardano();
  const { openModal, closeModal } = useModal();
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const navigate = useNavigate();

  const {
    control,
    handleSubmit,
    formState: { errors, isValid },
  } = useFormContext<UrlAndHashFormValues>();

  const watch = useWatch({
    control,
  });

  const isUrlNullOrFilledIn = watch.url !== "" && watch.url !== null;
  const isHashNullOrFilledIn = watch.hash !== "" && watch.hash !== null;
  const showSubmitButton = isUrlNullOrFilledIn || isHashNullOrFilledIn;

  const onSubmit = useCallback(
    async (values: UrlAndHashFormValues) => {
      const { url, hash } = values;

      const urlSubmitValue = url ?? "";
      const hashSubmitValue = hash ?? "";
      setIsLoading(true);

      try {
        const certBuilder = await buildDRepRegCert(
          urlSubmitValue,
          hashSubmitValue
        );
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
              title: "Registration Transaction Submitted!",
              message:
                "The confirmation of your registration might take a bit of time but you can track it using.",
              link: "https://adanordic.com/latest_transactions",
              buttonText: "Go to dashboard",
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
            title: "Oops!",
            message: errorMessage,
            buttonText: "Go to dashboard",
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
    isLoading,
    control,
    errors,
    isValid,
    showSubmitButton,
    submitForm: handleSubmit(onSubmit),
  };
};
