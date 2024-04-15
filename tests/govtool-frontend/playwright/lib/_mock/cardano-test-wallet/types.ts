export type HexString = string;

export interface CIP30Provider {
  apiVersion: string;
  enable: () => Promise<CIP30Instance | Cip95Instance>;
  icon: string;
  isEnabled: () => Promise<Boolean>;
  name: string;
  supportedExtensions: Record<string, number>[];
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

export type CardanoTestWallet = {
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
};

export type CardanoTestWalletConfig = {
  networkId?: number;
  walletName?: string;
  enableStakeSigning?: boolean;
  extraRegisteredPubStakeKeys?: string[];
  extraRewardAddresses?: string[];
};

export type KuberValue = {
  [policyId: string]: Record<string, BigInt | number> | BigInt | number;
};
