import { Page } from "@playwright/test";
import { StaticWallet } from "@types";

export async function importWallet(page: Page, wallet: StaticWallet) {
  await page.addInitScript((wallet) => {
    // @ts-ignore
    window.cardanoTestWallet.wallet = wallet;
  }, wallet);
}
