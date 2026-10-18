import { getPdfWalletStatus } from "@/utils/getPdfWalletStatus";
import {
  WALLET_LS_KEY,
  removeItemFromLocalStorage,
  setItemToLocalStorage,
} from "@/utils/localStorage";

const saveName = () => setItemToLocalStorage(`${WALLET_LS_KEY}_name`, "hd");

describe("getPdfWalletStatus", () => {
  afterEach(() => removeItemFromLocalStorage(`${WALLET_LS_KEY}_name`));

  it.each([
    [
      "R0: saved wallet, enable not started",
      { isEnabled: false, isEnableLoading: null },
      true,
      "connecting",
    ],
    [
      "R2: enabled, no address yet",
      { isEnabled: true, isEnableLoading: "hd" },
      true,
      "connecting",
    ],
    [
      "R3: address without stake key",
      { isEnabled: true, isEnableLoading: "hd", address: "addr1" },
      true,
      "connecting",
    ],
    [
      "R4: stake key set, enable still loading",
      {
        isEnabled: true,
        isEnableLoading: "hd",
        address: "addr1",
        stakeKey: "stake1",
      },
      true,
      "connecting",
    ],
    [
      "R5: enable finished",
      {
        isEnabled: true,
        isEnableLoading: null,
        address: "addr1",
        stakeKey: "stake1",
      },
      true,
      "ready",
    ],
    [
      "several stake keys, none chosen",
      { isEnabled: true, isEnableLoading: null, address: "addr1" },
      false,
      "connecting",
    ],
    [
      "enable in flight without a saved name",
      { isEnabled: false, isEnableLoading: "hd" },
      false,
      "connecting",
    ],
    [
      "no saved wallet",
      { isEnabled: false, isEnableLoading: null },
      false,
      "disconnected",
    ],
  ])("%s", (_label, wallet, hasSavedName, expected) => {
    if (hasSavedName) saveName();
    expect(getPdfWalletStatus(wallet)).toBe(expected);
  });

  it("reads the saved name on every call", () => {
    const wallet = { isEnabled: false, isEnableLoading: null };
    saveName();
    expect(getPdfWalletStatus(wallet)).toBe("connecting");
    removeItemFromLocalStorage(`${WALLET_LS_KEY}_name`);
    expect(getPdfWalletStatus(wallet)).toBe("disconnected");
  });
});
