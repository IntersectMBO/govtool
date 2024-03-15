import { useCallback, useEffect, useState } from "react";
import {
  setItemToLocalStorage,
  removeItemFromLocalStorage,
  getItemFromLocalStorage,
  PENDING_TRANSACTION_KEY,
} from "@utils";
import { getTransactionStatus } from "@services";
import { useTranslation } from "@hooks";
import { StatusModalState } from "@organisms";
import {
  setLimitedDelegationInterval,
  setLimitedDRepActionInterval,
} from "./walletUtils";
import { useModal, useSnackbar } from ".";
import { VoterInfo } from "@/models";

const TIME_TO_EXPIRE_TRANSACTION = 3 * 60 * 1000; // 3 MINUTES
const REFRESH_TIME = 15 * 1000; // 15 SECONDS

type UseTransactionsProps = {
  dRepID: string;
  isEnabled: boolean;
  setVoter: (key: VoterInfo | undefined) => void;
  stakeKey: string | undefined;
};

export type PendingTransaction =
  | Record<
    Exclude<TransactionType, "delegate" | "vote">,
    TransactionStateWithoutResource | null
  > &
  Record<
    Extract<TransactionType, "delegate" | "vote">,
    TransactionStateWithResource | null
  >;

export type TransactionStateWithoutResource = {
  type: Exclude<TransactionType, "delegate" | "vote">;
  transactionHash: string;
  time: Date;
  resourceId?: never;
};

export type TransactionStateWithResource = {
  type: Extract<TransactionType, "delegate" | "vote">;
  transactionHash: string;
  time: Date;
  resourceId: string;
};

export type TransactionState =
  | TransactionStateWithResource
  | TransactionStateWithoutResource;

export type TransactionType =
  | "delegate"
  | "createGovAction"
  | "registerAsDrep"
  | "registerAsSoleVoter"
  | "retireAsDrep"
  | "retireAsSoleVoter"
  | "updateMetaData"
  | "vote";

export const useTransactions = ({
  dRepID,
  isEnabled,
  setVoter,
  stakeKey,
}: UseTransactionsProps) => {
  const { t } = useTranslation();
  const { openModal, closeModal } = useModal<StatusModalState>();
  const { addSuccessAlert, addWarningAlert, addErrorAlert } = useSnackbar();

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
      setTransaction(fromLocalStorage);
    }
  }, [isEnabled, stakeKey]);

  // Check transactions status
  useEffect(() => {
    if (!transaction?.transactionHash) return;

    const { transactionHash, type, resourceId } = transaction;

    const checkTransaction = async () => {
      const status = await getTransactionStatus(transactionHash);

      const resetTransaction = () => {
        clearInterval(interval);
        removeItemFromLocalStorage(`${PENDING_TRANSACTION_KEY}_${stakeKey}`);
        setTransaction(null);
      };

      if (status.transactionConfirmed) {
        const isDRepAction =
          type === "registerAsDrep" ||
          type === "registerAsSoleVoter" ||
          type === "retireAsDrep" ||
          type === "retireAsSoleVoter";

        const isActionSuccesful =
          type === "delegate"
            ? await setLimitedDelegationInterval(dRepID, resourceId, stakeKey)
            : isDRepAction
              ? await setLimitedDRepActionInterval(dRepID, type, setVoter)
              : undefined;

        if (isEnabled) {
          if (isActionSuccesful) {
            if (isDRepAction || type === "delegate") {
              addWarningAlert(t(`alerts.${type}.warning`));
              resetTransaction();
              return;
            }
          }

          addSuccessAlert(t(`alerts.${type}.success`));
          resetTransaction();

          if (isTransactionExpired(transaction.time)) {
            addErrorAlert(t(`alerts.${type}.failed`));
            resetTransaction();
          }
        }
      }
    };

    let interval = setInterval(checkTransaction, REFRESH_TIME);
    checkTransaction();

    if (isEnabled && transaction) {
      addWarningAlert(t("alerts.transactionInProgress"), 10000);
    }
  }, [transaction]);

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
      time: new Date(),
      ...data,
    } as TransactionState;

    setTransaction(newTransaction);
    setItemToLocalStorage(
      `${PENDING_TRANSACTION_KEY}_${stakeKey}`,
      newTransaction
    );
  };

  return {
    isPendingTransaction,
    pendingTransaction,
    updateTransaction,
  };
};

const isTransactionExpired = (time: Date): boolean =>
  new Date().getTime() - time.getTime() > TIME_TO_EXPIRE_TRANSACTION;
