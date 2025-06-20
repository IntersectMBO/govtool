import environments from "@constants/environments";
import { Page, expect } from "@playwright/test";
import kuberService from "@services/kuberService";
import { LockInterceptor, LockInterceptorInfo } from "lib/lockInterceptor";
import convertBufferToHex from "./convertBufferToHex";
import { ShelleyWallet } from "./crypto";
import { uploadMetadataAndGetJsonHash } from "./metadata";
import { WalletAndAnchorType } from "@types";
import { Logger } from "@helpers/logger";
import { functionWaitedAssert } from "./waitedLoop";

/**
 * Polls the transaction status until it's resolved or times out.
 * address is used to release lock of that address
 */
export async function pollTransaction(
  txHash: string,
  lockInfo?: LockInterceptorInfo
) {
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

        if (!lockInfo) return;

        await LockInterceptor.releaseLock(
          lockInfo.initiator,
          lockInfo.lockId,
          `Task completed for:${lockInfo.lockId}`
        );
      } catch (err) {
        if (lockInfo) {
          const errorMessage = { lockInfo, error: JSON.stringify(err) };

          await LockInterceptor.releaseLock(
            lockInfo.initiator,
            lockInfo.lockId,
            `Task failure: \n${JSON.stringify(errorMessage)}`
          );
        }
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

export async function registerStakeForWallet(wallet: ShelleyWallet) {
  const { txId, lockInfo } = await kuberService.registerStake(
    convertBufferToHex(wallet.stakeKey.private),
    convertBufferToHex(wallet.stakeKey.pkh),
    convertBufferToHex(wallet.paymentKey.private),
    wallet.addressBech32(environments.networkId)
  );
  await pollTransaction(txId, lockInfo);
}

export async function transferAdaForWallet(
  wallet: ShelleyWallet,
  amount?: number
) {
  const { txId, lockInfo } = await kuberService.transferADA(
    [wallet.addressBech32(environments.networkId)],
    amount
  );
  await pollTransaction(txId, lockInfo);
}

export async function registerDRepForWallet(wallet: ShelleyWallet) {
  const dataHashAndUrl = await uploadMetadataAndGetJsonHash();
  const metadataAnchorAndWallet: WalletAndAnchorType = {
    ...dataHashAndUrl,
    wallet: wallet.json(),
  };
  const registrationRes = await kuberService.dRepRegistration(
    convertBufferToHex(wallet.dRepKey.private),
    convertBufferToHex(wallet.dRepKey.pkh),
    metadataAnchorAndWallet
  );
  await pollTransaction(registrationRes.txId, registrationRes.lockInfo);
}
