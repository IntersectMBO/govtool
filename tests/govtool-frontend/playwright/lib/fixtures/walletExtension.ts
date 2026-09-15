import { test as base } from "@playwright/test";
import { StaticWallet } from "@types";
import { importWallet } from "./importWallet";
import loadDemosExtension from "./loadExtension";
import { injectLogger } from "@helpers/page";

type WalletExtensionTestOptions = {
  wallet?: StaticWallet;
  enableStakeSigning: boolean;
  enableDRepSigning: boolean;
  supportedExtensions: Record<string, number>[];
};

export const test = base.extend<WalletExtensionTestOptions>({
  wallet: [null, { option: true }],

  enableStakeSigning: [true, { option: true }],
  enableDRepSigning: [false, { option: true }],

  supportedExtensions: [[{ cip: 95 }], { option: true }],

  page: async (
    {
      page,
      wallet,
      enableStakeSigning,
      supportedExtensions,
      enableDRepSigning,
    },
    use
  ) => {
    await loadDemosExtension(
      page,
      enableStakeSigning,
      enableDRepSigning,
      supportedExtensions
    );

    if (wallet) {
      await importWallet(page, wallet);
    }
    injectLogger(page);

    await use(page);
  },
});
