export type ChooseWalletModalProps = {
  onClose: () => void;
  isOpen: boolean;
  isWalletEnableLoading: boolean;
  onClickWalletButton: (walletName: string) => void;
  onClickShowSupportedWallets?: () => void;
};

export type WalletInfo = {
  icon: string;
  name: string;
  label?: string;
};

export type WalletButtonProps = WalletInfo & {
  onClick: () => void;
  dataTestId?: string;
  isLoading?: boolean;
};
