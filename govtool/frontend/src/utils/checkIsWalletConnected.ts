import { WALLET_LS_KEY, getItemFromLocalStorage } from "@/utils/localStorage";

export const checkIsWalletConnected = () => !!(
  getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`) &&
  getItemFromLocalStorage(`${WALLET_LS_KEY}_name`)
);
