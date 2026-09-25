import environments from "@constants/environments";
import { test } from "@fixtures/testWallet";
import { correctDRepDirectoryFormat } from "@helpers/adaFormat";
import { setAllureEpic } from "@helpers/allure";
import { skipIfMainnet } from "@helpers/cardano";
import { waitForTxConfirmation } from "@helpers/transaction";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import { expect } from "@playwright/test";
import { lovelaceBalance } from "lib/wallet/testWallets";

test.beforeEach(async () => {
  await setAllureEpic("2. Delegation");
  await skipIfMainnet();
});

test.describe("Abstain delegation", () => {
  test.use({ walletRole: "adaHolder03", walletFundsAda: 50 });

  test("2U. Should show delegated voting power to Abstain", async ({
    page,
    testWallet,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();

    await dRepDirectoryPage.automaticDelegationOptionsDropdown.click();
    await page.getByTestId("abstain-from-every-vote-delegate-button").click();
    await waitForTxConfirmation(page);

    const balance = Number(await lovelaceBalance(testWallet!)) / 1_000_000;

    await expect(
      page.getByText(
        `You have delegated ₳${correctDRepDirectoryFormat(balance)}`
      )
    ).toBeVisible({
      timeout: 60_000,
    });
  });
});
