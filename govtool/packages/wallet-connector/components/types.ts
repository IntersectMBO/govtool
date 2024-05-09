export type ChooseWalletModalProps = {
  handleClose: () => void;
  isOpen: boolean;
  isWalletEnableLoading: boolean;
  onClickShowSupportedWallets: () => void;
  onClickWalletButton: (walletName: string) => void;
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
