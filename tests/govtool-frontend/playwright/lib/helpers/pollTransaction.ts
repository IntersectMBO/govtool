import kuberService from "@services/kuberService";
import { expect } from "@playwright/test";

/**
 * Transaction takes at most 2 mins to complete.
 * Polls the transaction status until it's resolved or times out.
 */
export default async function pollTransaction(txHash: string) {
  return expect
    .poll(
      async () => {
        const response = await kuberService.getTransactionDetails(txHash);
        return response.status;
      },
      { message: `[Kuber Tx] success for tx: ${txHash}`, timeout: 2 * 60000 },
    )
    .toBe(200);
}
