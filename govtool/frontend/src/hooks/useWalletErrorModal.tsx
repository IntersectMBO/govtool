import { useModal } from "@context";
import { useTranslation } from "react-i18next";

type WalletErrorModalProps = {
  error: unknown;
  onSumbit?: () => void;
  title?: string;
  buttonText?: string;
  dataTestId?: string;
};

export const useWalletErrorModal = () => {
  const { openModal, closeModal } = useModal();
  const { t } = useTranslation();

  const openWalletErrorModal = ({
    error,
    onSumbit,
    title,
    buttonText,
    dataTestId,
  }: WalletErrorModalProps) => {
    const errorMessage =
      error && typeof error === "object" && "info" in error
        ? error.info
        : JSON.stringify(error, Object.getOwnPropertyNames(error));

    openModal({
      type: "statusModal",
      state: {
        status: "warning",
        title: title ?? t("modals.common.oops"),
        message: errorMessage,
        buttonText: buttonText ?? t("cancel"),
        onSubmit: () => {
          if (onSumbit) onSumbit();
          closeModal();
        },
        dataTestId: dataTestId ?? "wallet-error-modal",
      },
    });
  };

  return openWalletErrorModal;
};
