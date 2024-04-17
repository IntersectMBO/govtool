import { useModal } from "@context";
import { useTranslation } from "react-i18next";

type WalletErrorModalProps = {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  error: any;
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
    const errorMessage = error.info ? error.info : error;

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
