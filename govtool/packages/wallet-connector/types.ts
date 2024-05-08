import { ReactNode } from "react";

export type WalletContextProviderProps = {
  children: ReactNode;
};

export type WalletContextValues = {
  disableWallet: () => void;
  enableError: string | null;
  enableWallet: (walletName: string) => Promise<void>;
  isEnableLoading: boolean;
  walletAPI: TWalletAPI | null;
};

export type Extension = {
  cip: number;
};

export type TWalletAPI = {
  cip95: {
    getPubDRepKey(): Promise<string>;
    getRegisteredPubStakeKeys(): Promise<string[]>;
    getUnregisteredPubStakeKeys(): Promise<string[]>;
    signData(arg0: any, arg1: any): Promise<any>;
  };
  experimantal: {
    on(arg0: any, arg1: any): any;
    off(arg0: any, arg1: any): any;
    getCollateral(): any;
  };
  getBalance(): Promise<string>;
  getChangeAddress(): Promise<string>;
  getExtensions(): Promise<Extension[]>;
  getNetworkId(): Promise<number>;
  getRewardAddresses(): Promise<string[]>;
  getUnusedAddresses(): Promise<string[]>;
  getUsedAddresses(): Promise<string[]>;
  getUtxos(): Promise<string[]>;
  signData(arg0: any, arg1?: any): Promise<any>;
  signTx(arg0: any, arg1?: any): Promise<any>;
  submitTx(arg0: any): Promise<any>;
};

export type EnableExtensionPayload = {
  extensions: Extension[];
};

export type CardanoBrowserWallet = {
  apiVersion: string;
  enable(extensions?: EnableExtensionPayload): Promise<TWalletAPI>;
  icon: string;
  isEnabled(): Promise<boolean>;
  name: string;
  supportedExtensions: Extension[];
};

declare global {
  interface Window {
    cardano: {
      [key: string]: CardanoBrowserWallet;
    };
  }
}

export enum WalletConnectorErrors {
  NO_CIP30_SUPPORT = "%WALLET_NAME% wallet doesn't support CIP 30 extension.",
  NO_CIP95_SUPPORT = "%WALLET_NAME% wallet doesn't support CIP 95 extension.",
  USE_WALLET_CONTEXT_USED_WITHOUT_PROVIDER = "useWalletContext must be used in the WalletProvider.",
}
