import { useCallback, useEffect, useState } from "react";
import {
  setItemToLocalStorage,
  removeItemFromLocalStorage,
  getItemFromLocalStorage,
  PENDING_TRANSACTION_KEY,
  wait,
} from "@utils";
import { getTransactionStatus } from "@services";
import { useTranslation } from "@hooks";
import { StatusModalState } from "@organisms";
import { useQueryClient } from "@tanstack/react-query";
import { useModal, useSnackbar } from "..";
import { TransactionState } from "./types";
import { getDesiredResult, getQueryKey, refetchData } from "./utils";

const TIME_TO_EXPIRE_TRANSACTION = 3 * 60 * 1000; // 3 MINUTES
const TRANSACTION_REFRESH_TIME = 15 * 1000; // 15 SECONDS
const DB_SYNC_REFRESH_TIME = 3 * 1000; // 3 SECONDS
const DB_SYNC_MAX_ATTEMPTS = 10;

type UsePendingTransactionProps = {
  isEnabled: boolean;
  stakeKey: string | undefined;
};

export const usePendingTransaction = ({
  isEnabled,
  stakeKey,
}: UsePendingTransactionProps) => {
  const { t } = useTranslation();
  const { openModal, closeModal } = useModal<StatusModalState>();
  const { addSuccessAlert, addWarningAlert, addErrorAlert } = useSnackbar();
  const queryClient = useQueryClient();

  const [transaction, setTransaction] = useState<TransactionState | null>(null);

  const pendingTransaction = {
    delegate: transaction?.type === "delegate" ? transaction : null,
    createGovAction:
      transaction?.type === "createGovAction" ? transaction : null,
    registerAsDrep: transaction?.type === "registerAsDrep" ? transaction : null,
    registerAsDirectVoter:
      transaction?.type === "registerAsDirectVoter" ? transaction : null,
    retireAsDrep: transaction?.type === "retireAsDrep" ? transaction : null,
    retireAsDirectVoter:
      transaction?.type === "retireAsDirectVoter" ? transaction : null,
    updateMetaData: transaction?.type === "updateMetaData" ? transaction : null,
    vote: transaction?.type === "vote" ? transaction : null,
  };

  // Load transactions from local storage
  useEffect(() => {
    if (isEnabled) {
      const fromLocalStorage = getItemFromLocalStorage(
        `${PENDING_TRANSACTION_KEY}_${stakeKey}`,
      );
      if (!fromLocalStorage) setTransaction(null);
      else
        setTransaction({
          ...fromLocalStorage,
          resourceId: fromLocalStorage.resourceId ?? undefined,
        });
    }
  }, [isEnabled, stakeKey]);

  // Check transactions status
  useEffect(() => {
    if (!transaction?.transactionHash) return undefined;

    const { transactionHash, type, resourceId } = transaction;
    // Set once the transaction is resolved or this effect is torn down, so a
    // check still in flight cannot report again.
    let isSettled = false;
    // A check can outlast the polling interval while it waits for the backend
    // to catch up, so two never run at once.
    let isChecking = false;

    const resetTransaction = () => {
      isSettled = true;
      clearInterval(interval);
      removeItemFromLocalStorage(`${PENDING_TRANSACTION_KEY}_${stakeKey}`);
      setTransaction(null);
    };

    const checkTransaction = async () => {
      if (isSettled || isChecking) return;
      isChecking = true;
      try {
        const status = await getTransactionStatus(transactionHash);

        // A transaction on chain carries everything it did, so a confirmed
        // vote transaction has cast its votes. Some backends cannot list a
        // vote by its transaction and send an empty votingProcedure, so it is
        // not required here.
        if (status.transactionConfirmed && isEnabled) {
          const desiredResult = getDesiredResult(type, resourceId);
          const queryKey = getQueryKey(type, transaction);

          for (let count = 0; count < DB_SYNC_MAX_ATTEMPTS; count++) {
            if (isSettled) return;
            // eslint-disable-next-line no-await-in-loop
            const data = await refetchData(
              type,
              queryClient,
              queryKey,
              resourceId,
            );

            if (desiredResult === data) {
              addSuccessAlert(t(`alerts.${type}.success`));
              resetTransaction();
              return;
            }
            // eslint-disable-next-line no-await-in-loop
            await wait(DB_SYNC_REFRESH_TIME);
          }
        }

        // Still unresolved: keep polling until the change shows up or the
        // transaction expires, so "in progress" never stays up for good.
        if (!isSettled && isTransactionExpired(transaction.time)) {
          addErrorAlert(t(`alerts.${type}.failed`));
          resetTransaction();
        }
      } finally {
        isChecking = false;
      }
    };

    const interval = setInterval(checkTransaction, TRANSACTION_REFRESH_TIME);
    checkTransaction();

    if (isEnabled && transaction) {
      addWarningAlert(t("alerts.transactionInProgress"), 10000);
    }

    return () => {
      isSettled = true;
      clearInterval(interval);
    };
  }, [isEnabled, transaction]);

  const isPendingTransaction = useCallback(() => {
    if (transaction) {
      openModal({
        type: "statusModal",
        state: {
          status: "info",
          title: t("modals.waitForTransaction.title"),
          message: t("modals.waitForTransaction.message"),
          buttonText: t("ok"),
          onSubmit: () => {
            closeModal();
          },
          dataTestId: "transaction-inprogress-modal",
        },
      });
      return true;
    }
    return false;
  }, [closeModal, openModal, transaction]);

  const updateTransaction = (data: Omit<TransactionState, "time">) => {
    const newTransaction = {
      time: new Date().toISOString(),
      ...data,
    } as TransactionState;

    setTransaction(newTransaction);
    setItemToLocalStorage(`${PENDING_TRANSACTION_KEY}_${stakeKey}`, {
      ...newTransaction,
      resourceId: newTransaction.resourceId || null,
    });
  };

  return {
    isPendingTransaction,
    pendingTransaction,
    updateTransaction,
  };
};

const isTransactionExpired = (time: string): boolean =>
  Date.now() - new Date(time).getTime() > TIME_TO_EXPIRE_TRANSACTION;
