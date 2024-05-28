import { adaHolderWallets, dRepWallets } from "@constants/staticWallets";
import { setAllureEpic } from "@helpers/allure";
import { pollTransaction } from "@helpers/transaction";
import { expect, test as setup } from "@playwright/test";
import kuberService from "@services/kuberService";
import environments from "lib/constants/environments";

setup.describe.configure({ mode: "serial", timeout: environments.txTimeOut });

setup.beforeEach(async () => {
  await setAllureEpic("Setup");
});

setup("Initialize static wallets", async () => {
  try {
    const wallets = [...adaHolderWallets, ...dRepWallets];
    const res = await kuberService.initializeWallets(wallets);
    await pollTransaction(res.txId);
  } catch (err) {
    if (err.status === 400) {
      expect(true, "Wallets already initialized").toBeTruthy();
    } else {
      throw Error(err);
    }
  }
});

// for (const wallet of [...adaHolderWallets, ...dRepWallets]) {
//   setup(`Register stake of static wallets: ${wallet.address}`, async () => {
//     await setAllureStory("Register stake");
//     try {
//       const { txId, lockInfo } = await kuberService.registerStake(
//         wallet.stake.private,
//         wallet.stake.pkh,
//         wallet.payment.private,
//         wallet.address
//       );
//       await pollTransaction(txId, lockInfo);
//     } catch (err) {
//       if (err.status === 400) {
//         expect(true, "Stake already registered").toBeTruthy();
//       } else {
//         throw Error(err);
//       }
//     }
//   });
// }
