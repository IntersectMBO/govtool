import { test } from "@fixtures/walletExtension";
import { dRep01Wallet, user01Wallet } from "@constants/staticWallets";
import DelegationPage from "@pages/delegationPage";
import { expect } from "@playwright/test";
import generateRandomDRepId from "@helpers/generateRandomDRepId";

test.describe("Logged in specs", () => {
  test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

  test("2B. Should access delegation to dRep page @smoke @fast", async ({
    page,
  }) => {
    await page.goto("/");

    await page.getByTestId("delegate-button").click();
    await expect(page.getByText(/Delegate to DRep/).first()).toBeVisible();
  });

  test("2I. Should check validity of DRep Id @slow @regression", async ({
    page,
  }) => {
    const urlToIntercept = "**/utxo?**";
    const invalidDRepId = generateRandomDRepId();
    const validDRepId = dRep01Wallet.dRepId;

    // Invalidity checks
    const delegationPage = new DelegationPage(page);
    await delegationPage.goto();

    await delegationPage.delegateToDrep(invalidDRepId);
    await expect(delegationPage.delegationErrorModal).toBeVisible();

    await delegationPage.resetDRepForm();

    // Validity checks
    await delegationPage.dRepInput.fill(validDRepId);
    await delegationPage.delegateBtn.click();
    const response = await page.waitForResponse(urlToIntercept);
    expect(response.body.length).toEqual(0);
  });
});
