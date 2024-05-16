import { adaHolderWallets, dRepWallets } from "@constants/staticWallets";
import { pollTransaction } from "@helpers/transaction";
import { expect, test as setup } from "@playwright/test";
import kuberService from "@services/kuberService";
import environments from "lib/constants/environments";

setup.describe.configure({ mode: "serial", timeout: environments.txTimeOut });

setup("Fund static wallets", async () => {
  const addresses = [...adaHolderWallets, ...dRepWallets].map((e) => e.address);
  const res = await kuberService.transferADA(addresses);
  await pollTransaction(res.txId);
});

for (const wallet of [...adaHolderWallets, ...dRepWallets]) {
  setup(`Register stake of static wallet: ${wallet.address}`, async () => {
    try {
      const { txId, lockInfo } = await kuberService.registerStake(
        wallet.stake.private,
        wallet.stake.pkh,
        wallet.payment.private,
        wallet.address
      );
      await pollTransaction(txId, lockInfo);
    } catch (err) {
      if (err.status === 400) {
        expect(true, "Stake already registered").toBeTruthy();
      } else {
        throw Error(err);
      }
    }
  });
}
