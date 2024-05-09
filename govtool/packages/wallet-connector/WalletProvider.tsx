import React from "react";

import { WalletService } from "./WalletService";
import {
  TWalletAPI,
  WalletContextProviderProps,
  WalletConnectorErrors,
  WalletContextValues,
} from "./types";

const WalletContext = React.createContext<WalletContextValues | null>(null);

const WalletProvider = ({ children }: WalletContextProviderProps) => {
  const [isEnableLoading, setIsEnableLoading] = React.useState<boolean>(false);
  const [enableError, setEnableError] = React.useState<string | null>(null);
  const [walletAPI, setWalletAPI] = React.useState<TWalletAPI | null>(null);

  const enableWallet = async (walletName: string): Promise<void> => {
    setEnableError(null);
    setIsEnableLoading(true);
    try {
      const newWalletAPI = await WalletService.enableWallet(walletName);

      if (newWalletAPI) setWalletAPI(newWalletAPI);
    } catch (e) {
      setEnableError(e as string);
      throw e;
    } finally {
      setIsEnableLoading(false);
    }
  };

  const disableWallet = () => {
    setWalletAPI(null);
  };

  const value = React.useMemo(
    () => ({
      disableWallet,
      enableError,
      enableWallet,
      isEnableLoading,
      walletAPI,
    }),
    [disableWallet, enableError, enableWallet, isEnableLoading, walletAPI]
  );

  return (
    <WalletContext.Provider value={value}>{children}</WalletContext.Provider>
  );
};

const useWalletContext = (): WalletContextValues => {
  const context = React.useContext(WalletContext);
  if (!context)
    throw new Error(
      WalletConnectorErrors.USE_WALLET_CONTEXT_USED_WITHOUT_PROVIDER
    );

  return context;
};

export { useWalletContext, WalletProvider };
