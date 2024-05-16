import { importWallet } from "@fixtures/importWallet";
import loadDemosExtension from "@fixtures/loadExtension";
import { Browser, Page } from "@playwright/test";
import { ShelleyWallet } from "./crypto";

interface BrowserConfig {
  storageState: string;
  wallet: ShelleyWallet;
  enableStakeSigning?: boolean;
}

export async function createNewPageWithWallet(
  browser: Browser,
  { storageState, wallet, enableStakeSigning }: BrowserConfig
): Promise<Page> {
  const context = await browser.newContext({
    storageState: storageState,
  });
  const newPage = await context.newPage();

  await loadDemosExtension(newPage, enableStakeSigning);
  await importWallet(newPage, wallet.json());

  return newPage;
}
