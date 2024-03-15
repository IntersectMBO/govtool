import { useCallback, useEffect, useState } from "react";
import {
  DELEGATE_TRANSACTION_KEY,
  REGISTER_TRANSACTION_KEY,
  setItemToLocalStorage,
  removeItemFromLocalStorage,
  VOTE_TRANSACTION_KEY,
  REGISTER_SOLE_VOTER_TRANSACTION_KEY,
  getItemFromLocalStorage,
  GOVERNANCE_ACTION_KEY,
} from "@utils";
import { getTransactionStatus } from "@services";
import { useTranslation } from "@hooks";
import { DRepActionType } from "./wallet";
import { useModal, useSnackbar } from ".";
import {
  setLimitedDelegationInterval,
  setLimitedRegistrationInterval,
} from "./walletUtils";
import { StatusModalState } from "@/components/organisms";
import { VoterInfo } from "@/models";

const TIME_TO_EXPIRE_TRANSACTION = 3 * 60 * 1000; // 3 MINUTES
const REFRESH_TIME = 15 * 1000; // 15 SECONDS
const INTERVAL_TIME = 3000;
const ATTEMPTS_NUMBER = 10;

const EMPTY_TRANSACTIONS: TransactionsState = {
  delegate: {
    time: undefined,
    transactionHash: "",
  },
  govAction: {
    time: undefined,
    transactionHash: "",
  },
  register: {
    time: undefined,
    transactionHash: "",
    type: "",
  },
  soleVoter: {
    time: undefined,
    transactionHash: "",
    type: "",
  },
  vote: {
    time: undefined,
    transactionHash: "",
    proposalId: "",
  },
};

const TRANSACTION_KEYS = {
  delegate: DELEGATE_TRANSACTION_KEY,
  govAction: GOVERNANCE_ACTION_KEY,
  register: REGISTER_TRANSACTION_KEY,
  soleVoter: REGISTER_SOLE_VOTER_TRANSACTION_KEY,
  vote: VOTE_TRANSACTION_KEY,
};

type TransactionHistoryItem = {
  transactionHash: string;
  time?: Date;
};

type UseTransactionsProps = {
  delegateTo: string;
  dRepID: string;
  isEnabled: boolean;
  setVoter: (key: VoterInfo | undefined) => void;
  stakeKey: string | undefined;
};

type TransactionsState = {
  delegate: TransactionHistoryItem;
  govAction: TransactionHistoryItem;
  register: TransactionHistoryItem & { type: DRepActionType };
  soleVoter: TransactionHistoryItem & {
    type: Exclude<DRepActionType, "update">;
  };
  vote: TransactionHistoryItem & { proposalId: string };
};

export const useTransactions = ({
  delegateTo,
  dRepID,
  isEnabled,
  setVoter,
  stakeKey,
}: UseTransactionsProps) => {
  const { t } = useTranslation();
  const { openModal, closeModal } = useModal<StatusModalState>();
  const { addSuccessAlert, addWarningAlert, addErrorAlert } = useSnackbar();

  const [transactions, setTransactions] =
    useState<TransactionsState>(EMPTY_TRANSACTIONS);

  const isTransactonInProgress = Object.values(transactions).some(
    ({ transactionHash }) => transactionHash
  );

  // Load transactions from local storage
  useEffect(() => {
    const transactionFromLocalStorage = Object.entries(TRANSACTION_KEYS).reduce<
      Partial<TransactionsState>
    >((acc, [actionType, key]) => {
      const fromLocalStorage = getItemFromLocalStorage(`${key}_${stakeKey}`);
      if (!fromLocalStorage) return acc;
      return {
        ...acc,
        [actionType]: getItemFromLocalStorage(`${key}_${stakeKey}`),
      };
    }, {});

    setTransactions((prev) => ({
      ...prev,
      ...transactionFromLocalStorage,
    }));
  }, [isEnabled, stakeKey]);

  // Check transactions status
  useEffect(() => {
    Object.entries(transactions).forEach(([key, transaction]) => {
      if (!transaction.transactionHash) return;

      const actionType = key as keyof TransactionsState;

      const checkTransaction = async () => {
        const resetTransaction = () => {
          clearInterval(interval);
          removeItemFromLocalStorage(
            `${TRANSACTION_KEYS[actionType]}_${stakeKey}`
          );
          setTransactions((prev) => ({
            ...prev,
            [actionType]: EMPTY_TRANSACTIONS[actionType],
          }));
        };

        const status = await getTransactionStatus(transaction.transactionHash);

        if (status.transactionConfirmed) {
          if (isEnabled) {
            await onTransactionConfirmed(actionType, {
              delegateTo,
              dRepID,
              setVoter,
            });
          }
          resetTransaction();
        }

        if (isTransactionExpired(transaction)) {
          if (isEnabled) onTransactionExpired(actionType);
          resetTransaction();
        }
      };

      let interval = setInterval(checkTransaction, REFRESH_TIME);
      checkTransaction();
    });

    if (isEnabled && isTransactonInProgress) {
      addWarningAlert(t("alerts.transactionInProgress"), 10000);
    }
  }, [transactions]);

  const onTransactionConfirmed = async (
    actionType: keyof TransactionsState,
    {
      delegateTo,
      dRepID,
      setVoter,
    }: Pick<UseTransactionsProps, "delegateTo" | "dRepID" | "setVoter">
  ) => {
    if (actionType === "delegate") {
      await setLimitedDelegationInterval(
        INTERVAL_TIME,
        ATTEMPTS_NUMBER,
        dRepID,
        delegateTo,
        stakeKey
      ).then((isDelegated) =>
        (isDelegated
          ? addSuccessAlert(t("alerts.delegation.success"))
          : addWarningAlert(t("alerts.delegation.refreshPage"))));
    } else if (actionType === "govAction") {
      addSuccessAlert(t("alerts.govAction.success"));
    } else if (actionType === "register" || actionType === "soleVoter") {
      const { type } = transactions[actionType];
      if (type === "registration" || type === "retirement") {
        await setLimitedRegistrationInterval(
          INTERVAL_TIME,
          ATTEMPTS_NUMBER,
          dRepID,
          type,
          setVoter
        ).then((isRegistered) => {
          if (type === "registration") {
            const alertKey =
              actionType === "register"
                ? "registration"
                : "soleVoterRegistration";
            if (isRegistered) {
              addSuccessAlert(t(`alerts.${alertKey}.success`));
            } else {
              addWarningAlert(t(`alerts.${alertKey}.refreshPage`));
            }
          } else if (type === "retirement") {
            const alertKey =
              actionType === "register" ? "retirement" : "soleVoterRetirement";
            if (!isRegistered) {
              addSuccessAlert(t(`alerts.${alertKey}.success`));
            } else {
              addWarningAlert(t(`alerts.${alertKey}.refreshPage`));
            }
          }
        });
      } else {
        addSuccessAlert(t("alerts.metadataUpdate.success"));
      }
    } else if (actionType === "vote") {
      addSuccessAlert(t("alerts.voting.success"));
    }
  };

  const onTransactionExpired = (actionType: keyof TransactionsState) => {
    if (actionType === "delegate") {
      addErrorAlert(t("alerts.delegation.failed"));
    } else if (actionType === "govAction") {
      addErrorAlert(t("alerts.govAction.failed"));
    } else if (actionType === "register") {
      const { type } = transactions.register;
      if (!type) return;
      addErrorAlert(
        t(`alerts.${type === "update" ? "metadataUpdate" : type}.failed`)
      );
    } else if (actionType === "soleVoter") {
      const { type } = transactions.soleVoter;
      if (!type) return;
      addErrorAlert(t(`alerts.${type}.failed`));
    } else if (actionType === "vote") {
      addErrorAlert(t("alerts.voting.failed"));
    }
  };

  const isPendingTransaction = useCallback(() => {
    if (isTransactonInProgress) {
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
  }, [
    closeModal,
    openModal,
    transactions.delegate.transactionHash,
    transactions.register.transactionHash,
    transactions.soleVoter.transactionHash,
    transactions.vote.transactionHash,
  ]);

  const updateTransaction = ({
    transactionHash,
    type,
    proposalId,
    registrationType,
  }: {
    transactionHash: string;
    type?:
      | "delegation"
      | "govAction"
      | "registration"
      | "soleVoterRegistration"
      | "vote";
    proposalId?: string;
    registrationType?: DRepActionType;
  }) => {
    if (type === "soleVoterRegistration" && registrationType === "update") return;

    const transaction = {
      time: new Date(),
      transactionHash,
      ...(type === "vote" && { proposalId }),
      ...((type === "registration" || type === "soleVoterRegistration") && {
        type: registrationType ?? "",
      }),
    };

    const actionType =
      type === "delegation"
        ? "delegate"
        : type === "registration"
          ? "register"
          : type === "soleVoterRegistration"
            ? "soleVoter"
            : "vote";

    setTransactions((prev) => ({
      ...prev,
      [actionType]: transaction,
    }));
    setItemToLocalStorage(
      `${TRANSACTION_KEYS[actionType]}_${stakeKey}`,
      JSON.stringify(transaction)
    );
  };

  return {
    delegateTransaction: transactions.delegate,
    govActionTransaction: transactions.govAction,
    registerTransaction: transactions.register,
    soleVoterTransaction: transactions.soleVoter,
    voteTransaction: transactions.vote,
    isPendingTransaction,
    updateTransaction,
  };
};

const isTransactionExpired = (transaction: TransactionHistoryItem): boolean => {
  if (!transaction?.time) return true;
  return (
    new Date().getTime() - new Date(transaction.time).getTime() >
    TIME_TO_EXPIRE_TRANSACTION
  );
};
