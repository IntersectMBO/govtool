import { Logger } from "../lib/logger/logger";
import { ShelleyWallet } from "../lib/wallet/crypto";
import { store } from "./store";

const walletState = {
  setTxHash: (txHash: string) => {
    store.set("txHash", txHash);
    Logger.info(`[Wallet State] Tx Hash Saved: ${txHash}.`);
  },
  setWallet: (wallet?: ShelleyWallet) => {
    store.set("wallet", wallet);
  },
  setStakePkh: (pkh: string) => {
    store.set("stakePkh", pkh);
  },
  setSigningKey: (key: string) => {
    store.set("signingKey", key);
  },
  setAddress: (addr: string) => {
    store.set("address", addr);
  },
  getWallet: (): ShelleyWallet => store.get("wallet"),
  getAddress: () => store.get("address"),
  getSigningKey: () => store.get("signingKey"),
  getStakePkh: () => store.get("stakePkh"),
  getTxHash: () => {
    const txHash = store.get("txHash");
    Logger.info(`[Wallet State] Getting txHash: ${txHash}.`);
    return txHash;
  },
};

export default walletState;
