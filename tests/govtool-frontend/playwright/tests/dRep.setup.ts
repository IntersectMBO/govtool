import pollTransaction from "@helpers/pollTransaction";
import {
  adaHolderWallets,
  dRep01Wallet,
  dRepWallets,
} from "@constants/staticWallets";
import { expect, test as setup } from "@playwright/test";
import kuberService from "@services/kuberService";

setup("Register DRep", async () => {
  const res = await kuberService.dRepRegistration(
    dRep01Wallet.address,
    dRep01Wallet.payment.private,
    dRep01Wallet.stake.pkh
  );

  await pollTransaction(res.txId);
});
