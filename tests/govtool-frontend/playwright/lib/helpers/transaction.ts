import environments from "@constants/environments";
import { Page, expect } from "@playwright/test";
import kuberService from "@services/kuberService";
import { Logger } from "@helpers/logger";
import { functionWaitedAssert } from "./waitedLoop";

/**
 * Polls the transaction status until it's resolved or times out.
 */
export async function pollTransaction(txHash: string) {
  await functionWaitedAssert(
    async () => {
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
            }
          )
          .toBeGreaterThan(0);

        Logger.success("Tx completed");
      } catch (err) {
        Logger.fail(`Failed due to ${err}`);
        throw err;
      }
    },
    { timeout: environments.txTimeOut + 60_000, name: "pollTransaction" }
  );
}

export async function waitForTxConfirmation(
  page: Page,
  triggerCallback?: () => Promise<void>
) {
  let transactionHash: string | undefined;
  try {
    await triggerCallback?.call(this);
    const transactionStatusPromise = page.waitForRequest((request) => {
      return request.url().includes("/transaction/status/");
    });

    await expect(
      page
        .getByTestId("alert-warning")
        .getByText("Transaction in progress", { exact: false })
    ).toBeVisible({
      timeout: 90_000,
    });
    const url = (await transactionStatusPromise).url();
    const regex = /\/transaction\/status\/([^\/]+)$/;
    const match = url.match(regex);
    if (match) {
      transactionHash = match[1];
    }

    if (transactionHash) {
      await pollTransaction(transactionHash);
      await expect(
        page.getByText("In Progress", { exact: true }).first() //FIXME: Only one element needs to be displayed
      ).not.toBeVisible({ timeout: 90_000 });
    }
  } catch (error) {
    Logger.fail(error.message);
    throw new Error(error);
  }
}
