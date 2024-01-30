import { useCallback, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useForm, useWatch } from "react-hook-form";

import { PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import { useGetDRepListQuery, useTranslation } from "@hooks";
import { formHexToBech32 } from "@utils";

export interface DelegateTodrepFormValues {
  dRepId: string;
}

export const useDelegateTodRepForm = () => {
  const {
    setDelegatedDRepId,
    buildSignSubmitConwayCertTx,
    buildVoteDelegationCert,
  } = useCardano();
  const { data: drepList } = useGetDRepListQuery();
  const { openModal, closeModal, modal } = useModal();
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const navigate = useNavigate();
  const { t } = useTranslation();

  const { control, handleSubmit } = useForm<DelegateTodrepFormValues>();

  const watch = useWatch({
    control,
    name: "dRepId",
  });

  const isDelegateButtonDisabled = !watch;

  const delegate = useCallback(
    async ({ dRepId }: DelegateTodrepFormValues) => {
      setIsLoading(true);
      try {
        setDelegatedDRepId(dRepId);
        let isValidDrep = false;
        if (drepList?.length) {
          isValidDrep = drepList.some((i) => {
            return i.drepId === dRepId || formHexToBech32(i.drepId) === dRepId;
          });
        }
        if (!drepList?.length || !isValidDrep) {
          throw new Error(t("errors.dRepIdNotFound"));
        }
        const certBuilder = await buildVoteDelegationCert(dRepId);
        const result = await buildSignSubmitConwayCertTx({
          certBuilder,
          type: "delegation",
        });
        if (result)
          openModal({
            type: "statusModal",
            state: {
              status: "success",
              title: t("modals.delegation.title"),
              message: t("modals.delegation.message"),
              link: "https://adanordic.com/latest_transactions",
              buttonText: t("modals.common.goToDashboard"),
              onSubmit: () => {
                navigate(PATHS.dashboard);
                closeModal();
              },
              dataTestId: "delegation-transaction-submitted-modal",
            },
          });
      } catch (error) {
        openModal({
          type: "statusModal",
          state: {
            status: "warning",
            message: `${error}`.replace("Error: ", ""),
            onSubmit: () => {
              closeModal();
            },
            title: t("modals.common.oops"),
            dataTestId: "delegation-transaction-error-modal",
          },
        });
      } finally {
        setIsLoading(false);
      }
    },
    [buildVoteDelegationCert, buildSignSubmitConwayCertTx, drepList]
  );

  return {
    control,
    isDelegateButtonDisabled,
    delegate: handleSubmit(delegate),
    modal,
    isLoading,
  };
};
