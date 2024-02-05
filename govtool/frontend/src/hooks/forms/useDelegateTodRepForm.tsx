import { useCallback, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useForm, useWatch } from "react-hook-form";

import { PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import { useGetDRepListQuery } from "@hooks";
import { formHexToBech32 } from "@utils";

export interface DelegateTodrepFormValues {
  dRepID: string;
}

export const useDelegateTodRepForm = () => {
  const {
    setDelegatedDRepID,
    buildSignSubmitConwayCertTx,
    buildVoteDelegationCert,
  } = useCardano();
  const { data: drepList } = useGetDRepListQuery();
  const { openModal, closeModal, modal } = useModal();
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const navigate = useNavigate();

  const { control, handleSubmit } = useForm<DelegateTodrepFormValues>();

  const watch = useWatch({
    control,
    name: "dRepID",
  });

  const isDelegateButtonDisabled = !watch;

  const delegate = useCallback(
    async ({ dRepID }: DelegateTodrepFormValues) => {
      setIsLoading(true);
      try {
        setDelegatedDRepID(dRepID);
        let isValidDrep = false;
        if (drepList?.length) {
          isValidDrep = drepList.some((i) => {
            return i.drepId === dRepID || formHexToBech32(i.drepId) === dRepID;
          });
        }
        if (!drepList?.length || !isValidDrep) {
          throw new Error("DrepId not found");
        }
        const certBuilder = await buildVoteDelegationCert(dRepID);
        const result = await buildSignSubmitConwayCertTx({
          certBuilder,
          type: "delegation",
        });
        if (result)
          openModal({
            type: "statusModal",
            state: {
              status: "success",
              title: "Delegation Transaction Submitted!",
              message:
                "The confirmation of your actual delegation might take a bit of time but you can track it using.",
              link: "https://adanordic.com/latest_transactions",
              buttonText: "Go to dashboard",
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
            title: "Oops!",
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
    isDelegationLoading: isLoading,
  };
};
