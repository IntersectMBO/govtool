import { checkIsWalletConnected } from "..";
import {
  WALLET_LS_KEY,
  setItemToLocalStorage,
  removeItemFromLocalStorage,
} from "@/utils/localStorage";

describe("checkIsWalletConnected function", () => {
  it("returns false when wallet information is present in local storage", () => {
    setItemToLocalStorage(`${WALLET_LS_KEY}_name`, "Nami");
    setItemToLocalStorage(`${WALLET_LS_KEY}_stake_key`, "teststakekey");
    const isConnected = checkIsWalletConnected();

    expect(isConnected).toBe(false);
  });

  it("returns true when wallet information is missing in local storage", () => {
    removeItemFromLocalStorage(`${WALLET_LS_KEY}_name`);
    removeItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`);

    const isConnected = checkIsWalletConnected();

    expect(isConnected).toBe(true);
  });
});
