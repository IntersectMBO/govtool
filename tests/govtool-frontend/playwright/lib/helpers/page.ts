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

// A 404 here is the frontend's minute-by-minute maintenance check finding no
// notice, which is the normal state, not a page error.
const EXPECTED_404_PATHS = ["/maintenance.json"];

function isExpected404(msg: ConsoleMessage) {
  if (!msg.text().includes("status of 404")) return false;
  const { url } = msg.location();
  if (!url) return false;
  const { pathname } = new URL(url);
  return EXPECTED_404_PATHS.includes(pathname);
}

export function injectLogger(page: Page) {
  // @ts-ignore
  if (!page.isLoggerInjected) {
    page.on("console", (msg: ConsoleMessage) => {
      if (msg.type() === "error" && !isExpected404(msg)) {
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
