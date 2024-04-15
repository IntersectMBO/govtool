import { test as base, expect } from "@playwright/test";
import loadDemosExtension from "./loadExtension";
import { importWallet } from "./importWallet";
import { StaticWallet } from "@types";

type WalletExtensionTestOptions = {
  wallet?: StaticWallet;
};

export const test = base.extend<WalletExtensionTestOptions>({
  wallet: [null, { option: true }],

  page: async ({ page, wallet }, use) => {
    await loadDemosExtension(page);

    if (wallet) {
      await importWallet(page, wallet);
    }

    await use(page);
  },
});
