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
import { useQueryClient } from "react-query";
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
    registerAsSoleVoter:
      transaction?.type === "registerAsSoleVoter" ? transaction : null,
    retireAsDrep: transaction?.type === "retireAsDrep" ? transaction : null,
    retireAsSoleVoter:
      transaction?.type === "retireAsSoleVoter" ? transaction : null,
    updateMetaData: transaction?.type === "updateMetaData" ? transaction : null,
    vote: transaction?.type === "vote" ? transaction : null,
  };

  // Load transactions from local storage
  useEffect(() => {
    if (isEnabled) {
      const fromLocalStorage = getItemFromLocalStorage(
        `${PENDING_TRANSACTION_KEY}_${stakeKey}`
      );
      if (!fromLocalStorage) setTransaction(null);
      else setTransaction({
        ...fromLocalStorage,
        resourceId: fromLocalStorage.resourceId ?? undefined,
      });
    }
  }, [isEnabled, stakeKey]);

  // Check transactions status
  useEffect(() => {
    if (!transaction?.transactionHash) return;

    const { transactionHash, type, resourceId } = transaction;

    const checkTransaction = async () => {
      const status = await getTransactionStatus(transactionHash);

      const resetTransaction = () => {
        removeItemFromLocalStorage(`${PENDING_TRANSACTION_KEY}_${stakeKey}`);
        setTransaction(null);
      };

      if (status.transactionConfirmed) {
        clearInterval(interval);
        if (isEnabled) {
          const desiredResult = getDesiredResult(
            type,
            resourceId,
          );
          const queryKey = getQueryKey(type, transaction);

          let count = 0;
          let isDBSyncUpdated = false;
          while (!isDBSyncUpdated && count < DB_SYNC_MAX_ATTEMPTS) {
            count++;
            // eslint-disable-next-line no-await-in-loop
            const data = await refetchData(type, queryClient, queryKey);
            if (desiredResult === data) {
              addSuccessAlert(t(`alerts.${type}.success`));
              resetTransaction();
              isDBSyncUpdated = true;
            } else {
              // eslint-disable-next-line no-await-in-loop
              await wait(DB_SYNC_REFRESH_TIME);
            }
          }
        }
      }

      if (isTransactionExpired(transaction.time)) {
        addErrorAlert(t(`alerts.${type}.failed`));
        resetTransaction();
      }
    };

    let interval = setInterval(checkTransaction, TRANSACTION_REFRESH_TIME);
    checkTransaction();

    if (isEnabled && transaction) {
      addWarningAlert(t("alerts.transactionInProgress"), 10000);
    }
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
    setItemToLocalStorage(
      `${PENDING_TRANSACTION_KEY}_${stakeKey}`,
      {
        ...newTransaction,
        resourceId: newTransaction.resourceId || null,
      }
    );
  };

  return {
    isPendingTransaction,
    pendingTransaction,
    updateTransaction,
  };
};

const isTransactionExpired = (time: string): boolean =>
  Date.now() - new Date(time).getTime() > TIME_TO_EXPIRE_TRANSACTION;
