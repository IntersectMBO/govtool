import { test as base } from "@playwright/test";
import { StaticWallet } from "@types";
import { importWallet } from "./importWallet";
import loadDemosExtension from "./loadExtension";

type WalletExtensionTestOptions = {
  wallet?: StaticWallet;
  enableStakeSigning: boolean;
};

export const test = base.extend<WalletExtensionTestOptions>({
  wallet: [null, { option: true }],

  enableStakeSigning: [true, { option: true }],

  page: async ({ page, wallet, enableStakeSigning }, use) => {
    await loadDemosExtension(page, enableStakeSigning);

    if (wallet) {
      await importWallet(page, wallet);
    }

    await use(page);
  },
});
