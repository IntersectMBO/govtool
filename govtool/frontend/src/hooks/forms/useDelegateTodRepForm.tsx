import { useCallback, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useForm, useWatch } from "react-hook-form";

import { PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import { useGetDRepListQuery, useTranslation } from "@hooks";
import { formHexToBech32 } from "@utils";

export interface DelegateTodrepFormValues {
  dRepID: string;
}

export const useDelegateTodRepForm = () => {
  const { buildSignSubmitConwayCertTx, buildVoteDelegationCert } = useCardano();
  const { data: drepList } = useGetDRepListQuery();
  const { openModal, closeModal, modal } = useModal();
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const navigate = useNavigate();
  const { t } = useTranslation();

  const {
    control,
    formState: { errors },
    handleSubmit,
  } = useForm<DelegateTodrepFormValues>();

  const watch = useWatch({
    control,
    name: "dRepID",
  });

  const isDelegateButtonDisabled = !watch;

  const delegate = useCallback(
    async ({ dRepID }: DelegateTodrepFormValues) => {
      setIsLoading(true);
      try {
        let isValidDrep = false;
        if (drepList?.length) {
          isValidDrep = drepList.some(
            (dRep) =>
              dRep.drepId === dRepID || formHexToBech32(dRep.drepId) === dRepID,
          );
        }
        if (!drepList?.length || !isValidDrep) {
          throw new Error(t("errors.dRepIdNotFound"));
        }
        const certBuilder = await buildVoteDelegationCert(dRepID);
        const result = await buildSignSubmitConwayCertTx({
          certBuilder,
          type: "delegate",
          resourceId: dRepID,
        });
        if (result) {
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
        }
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
    [buildVoteDelegationCert, buildSignSubmitConwayCertTx, drepList],
  );

  return {
    control,
    delegate: handleSubmit(delegate),
    errors,
    isDelegateButtonDisabled,
    isDelegationLoading: isLoading,
    modal,
  };
};
