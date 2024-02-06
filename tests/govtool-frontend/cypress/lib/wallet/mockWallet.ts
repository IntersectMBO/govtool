import { randomBytes } from "crypto";
import { CIP30Provider, Cip95Instance, CipMethodOverride } from "./types";

export const mockUserStakeKeys = [
  "012f5dc3125b8a07981e6e50f5a671e2c6fbb26c3ffde1cd1dcaf40a7fe8f160",
  "012f5dc3115b8a07981e6e50f5a671e2c6fbb26c3ffde1cd1dcaf40a7fe8f160",
];
export const mockUserDrepKey = randomBytes(32).toString("hex");
export const mockUserAddress =
  "000fdc780023d8be7c9ff3a6bdc0d8d3b263bd0cc12448c40948efbf42e557890352095f1cf6fd2b7d1a28e3c3cb029f48cf34ff890a28d176";
export let networkId: number = Cypress.env("networkId");
networkId =
  (networkId as unknown as string).toLowerCase() == "testnet" || networkId == 0
    ? 0
    : 1;

const walletInstance: Cip95Instance & {
  experimental: Record<string, CallableFunction>;
} = {
  getBalance: async () => {
    return "10000000";
  },
  submitTx: async (tx) => {
    return;
  },
  getUtxos: async (amount, paginate) => {
    return [
      "8282582018BC892DF1A4CF0D1B388A684D287304A500D7EEDB4D35D5CAB7C8922503A6E0008258390036E0CF1E52E05EF92E52C7BC2A04493D6BAE481B8ACBAB12EC4300D7F9C9E87246D2F0373885896AD2804B7229673204CAC9208345C1EA5B1B00000002540BE400",
    ];
  },
  getUsedAddresses: async () => [mockUserAddress],
  getUnusedAddresses: async () => [],
  getChangeAddress: async () =>
    "000fdc780023d8be7c9ff3a6bdc0d8d3b263bd0cc12448c40948efbf42e557890352095f1cf6fd2b7d1a28e3c3cb029f48cf34ff890a28d176",
  getRewardAddresses: async () => [
    "e0e557890352095f1cf6fd2b7d1a28e3c3cb029f48cf34ff890a28d176",
  ],
  getNetworkId: async () => networkId,
  experimental: {
    on: (eventName, callback) => {
      return;
    },
    off: (eventName, callback) => {
      return;
    },
    getCollateral: () => {
      return "";
    },
  },
  // CIP-95 -----------------------------
  cip95: {
    getPubDRepKey: async () => mockUserDrepKey,
    getRegisteredPubStakeKeys: async () => mockUserStakeKeys,
    getUnregisteredPubStakeKeys: async () => mockUserStakeKeys,
  },
  getActivePubStakeKeys: async () => mockUserStakeKeys,
  // getUnregisteredPubStakeKeys: async () => mockUserStakeKeys,
  signTx: async (tx, partialSign) => {
    return "a10081825820b004cba76275ee90b44de0bee3edf2f69b77a3936c59879536bd0d3fcbc25e635840ae41c154e42fe3ad56ecc040f51e7488c5851b129194362a7d35c08f8c4ca86f7b8e1a4ba53ee59d6d4ee0a0b90816702f7af3877235e281fee122c4d21c7e04";
  },
  signData: async (address, payload) => {
    return "";
  },
  getExtensions: () => [{ cip: 95 }],
};
export function mkSingleStakeKeyWallet() {
  return mkWallet({ getUnregisteredPubStakeKeys: [mockUserStakeKeys[0]] });
}

export function mkWallet(overrides?: CipMethodOverride): CIP30Provider {
  let result = { ...walletInstance };
  if (overrides) {
    Object.keys(overrides).forEach((k) => {
      if (result[k]) {
        // console.log("Overriding", k, "to", typeof overrides[k], overrides[k]);
        result[k] = async () => overrides[k];
      }
      if (result.cip95 && result.cip95[k]) {
        result.cip95[k] = async () => overrides[k];
      }
    });
  }
  let enabled = false;
  return {
    apiVersion: "1.3.1",
    icon: "data:image/svg+xml,%3C%3Fxml version='1.0' encoding='utf-8'%3F%3E%3Csvg viewBox='0 0 500 500' xmlns='http://www.w3.org/2000/svg'%3E%3Crect x='309.36' y='12.441' width='121.115' height='472.347' style='fill: rgb(128  177  211)%3B'/%3E%3Cellipse style='fill: rgb(128  177  211)%3B' cx='231.272' cy='320.966' rx='171.791' ry='137.051'/%3E%3C/svg%3E",

    enable: async function () {
      enabled = true;
      return result;
    },
    isEnabled: async function () {
      return enabled;
    },
    name: "Demos",
    supportedExtensions: [
      {
        cip: 95,
      },
    ],
  };
}
