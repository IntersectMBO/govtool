import environments from "@constants/environments";
import {
  adaHolder01Wallet,
  adaHolder02Wallet,
  dRep01Wallet,
} from "@constants/staticWallets";
import { createTempDRepAuth } from "@datafactory/createAuth";
import { test } from "@fixtures/walletExtension";
import { ShelleyWallet } from "@helpers/crypto";
import { createNewPageWithWallet } from "@helpers/page";
import { pollTransaction, waitForTxConfirmation } from "@helpers/transaction";
import DelegationPage from "@pages/delegationPage";
import { expect } from "@playwright/test";
import kuberService from "@services/kuberService";

test.describe("Delegate to others", () => {
  test.use({
    storageState: ".auth/adaHolder01.json",
    wallet: adaHolder01Wallet,
  });

  test("2A. Should show delegated DRep Id on dashboard after delegation @slow @critical", async ({
    page,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const delegationPage = new DelegationPage(page);
    await delegationPage.goto();

    await delegationPage.delegateToDRep(
      "drep1qzw234c0ly8csamxf8hrhfahvzwpllh2ckuzzvl38d22wwxxquu",
    );

    page.goto("/");
    await expect(page.getByTestId("delegated-dRep-id")).toHaveText(
      dRep01Wallet.dRepId
    );
  });
});

test.describe("Delegate to myself", () => {
  test("2E. Should register as SoleVoter  @slow @critical", async ({
    page,
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const wallet = await ShelleyWallet.generate();
    const txRes = await kuberService.transferADA(
      [wallet.addressBech32(environments.networkId)],
      600
    );
    await pollTransaction(txRes.txId, txRes.lockInfo);
    const dRepAuth = await createTempDRepAuth(page, wallet);
    const dRepPage = await createNewPageWithWallet(browser, {
      storageState: dRepAuth,
      wallet,
      enableStakeSigning: true,
    });
    await dRepPage.goto("/");
    await dRepPage.getByTestId("register-as-sole-voter-button").click();
    await dRepPage.getByTestId("retire-button").click(); // BUG: Incorrect test-id , it should be continue-retirement
    await expect(
      dRepPage.getByTestId("registration-transaction-submitted-modal")
    ).toBeVisible();
    dRepPage.getByTestId("confirm-modal-button").click();
    await waitForTxConfirmation(dRepPage);

    await expect(dRepPage.getByText("You are a Sole Voter")).toBeVisible();
  });
});

test.describe("Change Delegation", () => {
  test.use({
    storageState: ".auth/adaHolder02.json",
    wallet: adaHolder02Wallet,
  });

  // Skipped: Blocked because delegation is not working
  test.skip("2F. Should change delegated dRep @slow @critical", async ({
    page,
  }) => {
    const delegationPage = new DelegationPage(page);
    await delegationPage.goto();
    await delegationPage.delegateToDRep(dRep01Wallet.dRepId);

    // await delegationPage.goto("/");
    // await adaHolderPage.getByTestId("change-dRep-button").click();
    // await delegationPage.delegateToDRep(dRep02Wallet.dRepId);
    // await waitForTxConfirmation(page);
  });
});
