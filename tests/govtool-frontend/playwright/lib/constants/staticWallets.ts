const staticWallets: StaticWallet[] = require("../_mock/wallets.json");
import { StaticWallet } from "@types";

export const faucetWallet = staticWallets[0];

export const dRep01Wallet = staticWallets[1];
export const dRep02Wallet = staticWallets[2];

export const adaHolder01Wallet = staticWallets[3];
export const adaHolder02Wallet = staticWallets[4];

// Does not takes part in transaction
export const user01Wallet: StaticWallet = staticWallets[5];

export const adaHolderWallets = [adaHolder01Wallet, adaHolder02Wallet];

export const userWallets = [user01Wallet];

export const dRepWallets = [dRep01Wallet];
