import { adaHolder02Wallet, dRep01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import { waitForTxConfirmation } from "@helpers/transaction";
import DelegationPage from "@pages/delegationPage";

test.use({ storageState: ".auth/adaHolder02.json", wallet: adaHolder02Wallet });

// Skipped: Blocked because delegation is not working
test.skip("2F. Should change delegated dRep @slow @critical", async ({
  page,
}) => {
  const delegationPage = new DelegationPage(page);
  await delegationPage.goto();
  delegationPage.delegateToDRep(dRep01Wallet.dRepId);
  await waitForTxConfirmation(page);

  // await delegationPage.goto("/");
  // await adaHolderPage.getByTestId("change-dRep-button").click();
  // await delegationPage.delegateToDRep(dRep02Wallet.dRepId);
  // await waitForTxConfirmation(page);
});
