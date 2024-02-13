import { ShelleyWallet } from "./crypto";

export type HexString = string;
export type StaticWallet = {
  type: string;
  payment: {
    private: string;
    public: string;
    pkh: string;
  };
  stake: {
    private: string;
    public: string;
    pkh: string;
  };
  dRepId: string;
  address: string;
};

export interface CIP30Provider {
  apiVersion: string;
  enable: () => Promise<CIP30Instance | Cip95Instance>;
  icon: string;
  isEnabled: () => Promise<Boolean>;
  name: string;
  supportedExtensions: Record<string, number>[];
}

export enum Network {
  Mainnet = 0,
  Testnet = 1,
}

export interface CIP30Instance {
  submitTx: (tx: string) => Promise<string>;
  signTx: (tx: string, partial?: Boolean) => Promise<HexString>;
  getChangeAddress: () => Promise<HexString>;
  getNetworkId: () => Promise<number>;
  getRewardAddresses: () => Promise<HexString[]>;
  getUnusedAddresses: () => Promise<HexString[]>;
  getUsedAddresses: () => Promise<Array<HexString>>;
  getUtxos: (amount?: object, paginate?: any) => Promise<Array<HexString>>;
  getCollateral?: () => Promise<Array<HexString>>;
  signData: (address: string, payload: HexString) => Promise<HexString>;
  getBalance: () => Promise<string>;
}
export interface Cip95Instance extends CIP30Instance {
  cip95: {
    getPubDRepKey: () => Promise<HexString>;
    getUnregisteredPubStakeKeys: () => Promise<HexString[]>;
    getRegisteredPubStakeKeys: () => Promise<HexString[]>;
  };
  getActivePubStakeKeys: () => Promise<HexString[]>;
  getExtensions: () => Record<string, number>[];
}

export interface CipMethodOverride {
  submitTx?: HexString;
  signTx?: HexString;
  getChangeAddress?: HexString;
  getNetworkId?: number;
  getRewardAddresses?: HexString[];
  getUnusedAddresses?: HexString[];
  getUsedAddresses?: HexString[];
  getUtxos?: HexString[];
  getCollateral?: HexString[];
  signData?: HexString;
  getBalance?: string;
  getPubDRepKey?: HexString;
  // getUnregisteredPubStakeKeys?: HexString[];
  getExtensions?: Record<string, number>[];
  getRegisteredPubStakeKeys?: HexString[];
  getUnregisteredPubStakeKeys?: HexString[];
}

export interface WalletParams {
  loadFundsAndRegister?: boolean;
  onTxSign?: (txId: string) => void;
  testTx?: any;
  withStakeSigning?: boolean;
  onWalletInitialized?: ({
    wallet,
    stakePkh,
    signingKey,
    address,
  }: {
    wallet?: ShelleyWallet;
    stakePkh: string;
    signingKey: string;
    address: string;
  }) => void;
}
