import { CardanoTestWallet } from "@cardanoapi/cardano-test-wallet/types";
import { importWallet } from "@fixtures/importWallet";
import loadDemosExtension from "@fixtures/loadExtension";
import { Browser, Page } from "@playwright/test";
import { StaticWallet } from "@types";

interface NewPageConfig {
  storageState?: string;
  wallet: StaticWallet;
  enableStakeSigning?: boolean;
  supportedExtensions?: Record<string, number>[];
}

export async function createNewPageWithWallet(
  browser: Browser,
  newPageConfig: NewPageConfig
): Promise<Page> {
  const { storageState, wallet, ...extensionConfig } = newPageConfig;

  const context = await browser.newContext({
    storageState,
  });
  const newPage = await context.newPage();

  await loadDemosExtension(
    newPage,
    extensionConfig.enableStakeSigning,
    extensionConfig.supportedExtensions
  );
  await importWallet(newPage, wallet);

  return newPage;
}
