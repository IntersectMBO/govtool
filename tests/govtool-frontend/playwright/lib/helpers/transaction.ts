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

/**
 * Runs `triggerCallback` and waits until the transaction is on chain and the
 * app no longer shows it in progress. The hash comes from the first status
 * check the app makes after the callback starts, so the callback must submit
 * the transaction: the app checks as soon as it submits, and on the devnet a
 * block can include the transaction before that check, which is then the
 * last one.
 */
export async function waitForTxConfirmation(
  page: Page,
  triggerCallback: () => Promise<unknown>
) {
  let transactionHash: string | undefined;
  try {
    const transactionStatusPromise = page.waitForRequest((request) => {
      return request.url().includes("/transaction/status/");
    });
    await triggerCallback();

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
