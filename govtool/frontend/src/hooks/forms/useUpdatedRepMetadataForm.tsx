import { useCallback, useState } from "react";
import {
  UrlAndHashFormValues,
  useUrlAndHashFormController,
} from "./useUrlAndHashFormController";
import { useNavigate } from "react-router-dom";

import { PATHS } from "@consts";
import { useCardano, useSnackbar } from "@context";
import { useTranslation } from "@hooks";

export const useUpdatedRepMetadataForm = () => {
  const { buildSignSubmitConwayCertTx, buildDRepUpdateCert } = useCardano();
  const { addSuccessAlert, addErrorAlert } = useSnackbar();
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const navigate = useNavigate();
  const { t } = useTranslation();

  const {
    handleSubmit,
    control,
    formState: { errors, isValid },
  } = useUrlAndHashFormController();

  const onSubmit = useCallback(
    async (values: UrlAndHashFormValues) => {
      const { url, hash } = values;

      const urlSubmitValue = url ?? "";
      const hashSubmitValue = hash ?? "";
      setIsLoading(true);
      try {
        const certBuilder = await buildDRepUpdateCert(
          urlSubmitValue,
          hashSubmitValue
        );
        const result = await buildSignSubmitConwayCertTx({
          certBuilder,
          type: "registration",
          registrationType: "update",
        });
        if (result) addSuccessAlert(t("alerts.metadataUpdate.success"));
        navigate(PATHS.dashboard);
      } catch (e) {
        addErrorAlert(t("alerts.metadataUpdate.failed"));
      } finally {
        setIsLoading(false);
      }
    },
    [buildDRepUpdateCert, buildSignSubmitConwayCertTx]
  );

  return {
    submitForm: handleSubmit(onSubmit),
    control,
    errors,
    isValid,
    isLoading,
  };
};
