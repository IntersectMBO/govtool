import environments from "@constants/environments";
import { Page, expect } from "@playwright/test";
import kuberService from "@services/kuberService";
import { LockInterceptor } from "lib/lockInterceptor";
import { Logger } from "../../../cypress/lib/logger/logger";

/**
 * Polls the transaction status until it's resolved or times out.
 * address is used to release lock of that address
 */
export async function pollTransaction(txHash: string, address?: string) {
  try {
    Logger.info(`Waiting for tx completion: ${txHash}`);
    await expect
      .poll(
        async () => {
          const response = await kuberService.getTransactionDetails(txHash);
          const data = await response.json();
          return data.length;
        },
        { message: "Transaction failed", timeout: environments.txTimeOut }
      )
      .toBeGreaterThan(0);

    Logger.success("Tx completed");
  } catch (err) {
    throw err;
  } finally {
    if (!address) return;

    await LockInterceptor.releaseLockForAddress(address);
  }
}

export async function waitForTxConfirmation(page: Page) {
  let transactionHash: string | undefined;
  const transactionStatusPromise = page.waitForRequest((request) => {
    return request.url().includes("/transaction/status/");
  });

  const url = (await transactionStatusPromise).url();
  const regex = /\/transaction\/status\/([^\/]+)$/;
  const match = url.match(regex);
  if (match) {
    transactionHash = match[1];
  }

  if (transactionHash) {
    await pollTransaction(transactionHash);
    await page.reload();
  }
}
