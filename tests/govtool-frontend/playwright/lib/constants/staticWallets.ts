const staticWallets: StaticWallet[] = require("../_mock/wallets.json");
import { StaticWallet } from "@types";
import { proposalFaucetWallet } from "./proposalFaucetWallet";
export const faucetWallet = staticWallets[0];

export const dRep01Wallet = staticWallets[1];
export const dRep02Wallet = staticWallets[2];
export const dRep03Wallet = staticWallets[3];

export const adaHolder01Wallet = staticWallets[4];
export const adaHolder02Wallet = staticWallets[5];
export const adaHolder03Wallet = staticWallets[6];
export const adaHolder04Wallet = staticWallets[7];
export const adaHolder05Wallet = staticWallets[8];
export const adaHolder06Wallet = staticWallets[9];

// Does not takes part in transaction
export const user01Wallet: StaticWallet = staticWallets[10];

// Username is already set
export const proposal01Wallet: StaticWallet = staticWallets[11];
export const proposal02Wallet: StaticWallet = staticWallets[12];
export const proposal03Wallet: StaticWallet = staticWallets[13];
export const proposal04Wallet: StaticWallet = staticWallets[14];
export const proposal05Wallet: StaticWallet = staticWallets[15];
export const proposal06Wallet: StaticWallet = staticWallets[16];
export const proposal07Wallet: StaticWallet = staticWallets[17];
export const proposal08Wallet: StaticWallet = staticWallets[18];
export const proposal09Wallet: StaticWallet = staticWallets[19];

export const budgetProposal01Wallet: StaticWallet = staticWallets[20];
export const budgetProposal02Wallet: StaticWallet = staticWallets[21];
export const budgetProposal03Wallet: StaticWallet = staticWallets[22];
export const budgetProposal04Wallet: StaticWallet = staticWallets[23];

export const adaHolderWallets = [
  adaHolder01Wallet,
  adaHolder02Wallet,
  adaHolder03Wallet,
  adaHolder04Wallet,
  adaHolder05Wallet,
  adaHolder06Wallet,
];

export const userWallets = [user01Wallet];

export const dRepWallets = [dRep01Wallet, dRep02Wallet];
export const budgetProposalDRepWallets = [dRep03Wallet];

export const proposalWallets = [
  proposal01Wallet,
  proposal02Wallet,
  proposal03Wallet,
  proposal04Wallet,
  proposal05Wallet,
  proposal06Wallet,
  proposal07Wallet,
  proposal08Wallet,
  proposal09Wallet,
];

export const allStaticWallets = [
  ...dRepWallets,
  ...adaHolderWallets,
  user01Wallet,
  ...proposalWallets,
  faucetWallet,
  proposalFaucetWallet,
  ...budgetProposalDRepWallets,
];
