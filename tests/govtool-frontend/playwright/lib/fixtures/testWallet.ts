import { injectLogger } from "@helpers/page";
import { test as base } from "@playwright/test";
import { SimpleCip30Wallet } from "libcardano-wallet";
import { connectTestWallet } from "lib/wallet/pageWallet";
import { ensureFunded, roleWallet, WalletRole } from "lib/wallet/testWallets";

type TestWalletOptions = {
  /** The role whose HD account the page's wallet uses. */
  walletRole?: WalletRole;
  /** The balance the wallet is topped up to before the test. */
  walletFundsAda: number;
  testWallet?: SimpleCip30Wallet;
};

export const test = base.extend<TestWalletOptions>({
  walletRole: [undefined, { option: true }],
  walletFundsAda: [100, { option: true }],

  testWallet: async ({ walletRole, walletFundsAda }, use) => {
    if (!walletRole) return use(undefined);
    const wallet = await roleWallet(walletRole);
    await ensureFunded(wallet, walletFundsAda);
    await use(wallet);
  },

  page: async ({ page, testWallet }, use) => {
    if (testWallet) await connectTestWallet(page, testWallet);
    injectLogger(page);
    await use(page);
  },
});
