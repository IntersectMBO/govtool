import environments from "@constants/environments";
import { Page, expect } from "@playwright/test";
import kuberService from "@services/kuberService";
import { LockInterceptor, LockInterceptorInfo } from "lib/lockInterceptor";
import { Logger } from "../../../cypress/lib/logger/logger";

/**
 * Polls the transaction status until it's resolved or times out.
 * address is used to release lock of that address
 */
export async function pollTransaction(
  txHash: string,
  lockInfo?: LockInterceptorInfo,
) {
  try {
    Logger.info(`Waiting for tx completion: ${txHash}`);
    await expect
      .poll(
        async () => {
          const response = await kuberService.getTransactionDetails(txHash);
          const data = await response.json();
          return data.length;
        },
        {
          timeout: environments.txTimeOut,
        },
      )
      .toBeGreaterThan(0);

    Logger.success("Tx completed");

    if (!lockInfo) return;

    await LockInterceptor.releaseLockForAddress(
      lockInfo.address,
      lockInfo.lockId,
      `Task completed for:${lockInfo.lockId}`,
    );
  } catch (err) {
    if (lockInfo) {
      const errorMessage = { lockInfo, error: JSON.stringify(err) };

      await LockInterceptor.releaseLockForAddress(
        lockInfo.address,
        lockInfo.lockId,
        `Task failure: \n${JSON.stringify(errorMessage)}`,
      );
    }

    throw err;
  }
}

export async function waitForTxConfirmation(
  page: Page,
  triggerCallback?: () => Promise<void>,
) {
  let transactionHash: string | undefined;
  const transactionStatusPromise = page.waitForRequest((request) => {
    return request.url().includes("/transaction/status/");
  });

  await triggerCallback?.call(this);
  await expect(
    page
      .getByTestId("alert-warning")
      .getByText("Transaction in progress", { exact: false }),
  ).toBeVisible({
    timeout: 10000,
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
