import { injectLogger } from "@helpers/page";
import { test as base } from "@playwright/test";
import { connectTestWallet, PageWalletOptions } from "lib/wallet/pageWallet";
import { ensureFunded, testWallet, TestWallet } from "lib/wallet/testWallets";

type WalletOptions = {
  /** Name of the test wallet the page connects; none leaves the page disconnected. */
  walletName?: string;
  /** Balance the wallet is topped up to before the test; 0 skips funding. */
  walletFundsAda: number;
  /** How the page's wallet presents itself (extensions, extra stake keys). */
  pageWallet: PageWalletOptions;
  /** The connected test wallet, when `walletName` is set. */
  wallet?: TestWallet;
};

export const test = base.extend<WalletOptions>({
  walletName: [undefined, { option: true }],
  walletFundsAda: [50, { option: true }],
  pageWallet: [{}, { option: true }],

  wallet: async ({ walletName, walletFundsAda }, use) => {
    if (!walletName) return use(undefined);
    const wallet = await testWallet(walletName);
    if (walletFundsAda > 0) await ensureFunded(wallet, walletFundsAda);
    await use(wallet);
  },

  page: async ({ page, wallet, pageWallet }, use) => {
    if (wallet) await connectTestWallet(page, wallet, pageWallet);
    injectLogger(page);
    await use(page);
  },
});
