import { CardanoTestWallet } from "@cardanoapi/cardano-test-wallet/types";
import { Page } from "@playwright/test";
import { StaticWallet } from "@types";

export async function importWallet(
  page: Page,
  wallet: StaticWallet | CardanoTestWallet,
) {
  await page.addInitScript((wallet) => {
    // @ts-ignore
    window.cardanoTestWallet.wallet = wallet;
    //@ts-ignore
  }, wallet);
}
