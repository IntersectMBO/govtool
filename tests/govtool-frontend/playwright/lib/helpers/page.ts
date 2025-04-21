import { importWallet } from "@fixtures/importWallet";
import loadDemosExtension from "@fixtures/loadExtension";
import { Browser, ConsoleMessage, Page } from "@playwright/test";
import { StaticWallet } from "@types";
import { Logger } from "./logger";

interface NewPageConfig {
  storageState?: string;
  wallet: StaticWallet;
  enableStakeSigning?: boolean;
  enableDRepSigning?: boolean;
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
    extensionConfig.enableDRepSigning,
    extensionConfig.supportedExtensions
  );
  await importWallet(newPage, wallet);

  injectLogger(newPage);

  return newPage;
}

export function injectLogger(page: Page) {
  // @ts-ignore
  if (!page.isLoggerInjected) {
    page.on("console", (msg: ConsoleMessage) => {
      if (msg.type() === "error") {
        Logger.fail(msg.text());
      }
    });
    // @ts-ignore
    page.isLoggerInjected = true;
  }
}
