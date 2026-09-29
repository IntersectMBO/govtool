import environments from "@constants/environments";
import { injectLogger } from "@helpers/page";
import { test as base } from "@playwright/test";
import { connectTestWallet, PageWalletOptions } from "lib/wallet/pageWallet";
import {
  ensureFunded,
  singleUseWalletName,
  testWallet,
  TestWallet,
} from "lib/wallet/testWallets";
import { ensureStakeRegistered } from "lib/wallet/transactions";

type WalletOptions = {
  /** Name of the test wallet the page connects; none leaves the page disconnected. */
  walletName?: string;
  /**
   * Use a new account for `walletName` in each test process (see
   * singleUseWalletName), for tests that need a wallet with no chain history
   * and change its state. Default false: the name picks the same account for
   * the whole run, and a rerun with the same HD_RUN_ID.
   */
  singleUseWallet: boolean;
  /** Balance the wallet is topped up to before the test; 0 skips funding. */
  walletFundsAda: number;
  /**
   * Register the wallet's stake key before the test, as a wallet that has been
   * used on chain before. Default: when the wallet is funded, which is the
   * state the old static wallets had. False leaves a new wallet unregistered,
   * so the app registers the key in its first transaction.
   */
  stakeRegistered?: boolean;
  /** How the page's wallet presents itself (extensions, extra stake keys). */
  pageWallet: PageWalletOptions;
  /** The connected test wallet, when `walletName` is set. */
  wallet?: TestWallet;
};

export const test = base.extend<WalletOptions>({
  walletName: [undefined, { option: true }],
  singleUseWallet: [false, { option: true }],
  walletFundsAda: [50, { option: true }],
  stakeRegistered: [undefined, { option: true }],
  pageWallet: [{}, { option: true }],

  // Funding and stake registration are transactions: they get their own
  // timeout rather than eating into the test's.
  wallet: [
    async ({ walletName, singleUseWallet, walletFundsAda, stakeRegistered }, use) => {
      if (!walletName) return use(undefined);
      const wallet = await testWallet(
        singleUseWallet ? singleUseWalletName(walletName) : walletName
      );
      if (walletFundsAda > 0) await ensureFunded(wallet, walletFundsAda);
      if (stakeRegistered ?? walletFundsAda > 0) {
        await ensureStakeRegistered(wallet);
      }
      await use(wallet);
    },
    { timeout: 3 * environments.txTimeOut },
  ],

  page: async ({ page, wallet, pageWallet }, use) => {
    if (wallet) await connectTestWallet(page, wallet, pageWallet);
    injectLogger(page);
    await use(page);
  },
});
