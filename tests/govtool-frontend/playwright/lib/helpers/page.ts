import { Browser, ConsoleMessage, Page } from "@playwright/test";
import { connectTestWallet, PageWalletOptions } from "lib/wallet/pageWallet";
import { adaBalance, TestWallet } from "lib/wallet/testWallets";
import { Logger } from "./logger";

/** A page in a new browser context, with the test wallet connected. */
export async function createNewPageWithWallet(
  browser: Browser,
  { wallet, ...pageWallet }: { wallet: TestWallet } & PageWalletOptions
): Promise<Page> {
  const context = await browser.newContext();
  const newPage = await context.newPage();
  await connectTestWallet(newPage, wallet, pageWallet);
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

export async function logWalletDetails(address: string) {
  try {
    const balance = await adaBalance(address);
    console.log("wallet balance", balance);
  } catch (error) {
    console.log("failed to get balance", error);
  }
  console.log("wallet address", address);
}
