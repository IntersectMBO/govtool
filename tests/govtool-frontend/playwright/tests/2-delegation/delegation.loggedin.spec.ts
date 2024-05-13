import { user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import DelegationPage from "@pages/delegationPage";
import { expect } from "@playwright/test";

test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

test("2B. Should access delegation to dRep page @smoke @fast", async ({
  page,
}) => {
  await page.goto("/");

  await page.getByTestId("delegate-button").click(); // BUG incorrect test ID
  await expect(
    page.getByRole("navigation").getByText("DRep Directory")
  ).toBeVisible();
});

// Skipped: No need to insert dRep id to delegate
test.skip("2I. Should check validity of DRep Id @slow", async ({ page }) => {
  // const urlToIntercept = "**/utxo?**";
  // const invalidDRepId = generateRandomDRepId();
  // const validDRepId = dRep01Wallet.dRepId;
  // // Invalidity checks
  // const delegationPage = new DelegationPage(page);
  // await delegationPage.goto();
  // await delegationPage.delegateToDRep(invalidDRepId);
  // await expect(delegationPage.delegationErrorModal).toBeVisible();
  // await delegationPage.resetDRepForm();
  // // Validity checks
  // await delegationPage.dRepInput.fill(validDRepId);
  // await delegationPage.delegateBtn.click();
  // const response = await page.waitForResponse(urlToIntercept);
  // expect(response.body.length).toEqual(0);
});

test("2D. Verify Delegation Behavior in Connected State @smoke @fast", async ({
  page,
}) => {
  const delegationPage = new DelegationPage(page);
  await delegationPage.goto();

  // Verifying delegation options
  await delegationPage.delegationOptionsDropdown.click();
  await expect(delegationPage.signalNoConfidenceCard).toBeVisible();
  await expect(delegationPage.abstainDelegationCard).toBeVisible();

  expect(await delegationPage.delegateBtns.count()).toBeGreaterThanOrEqual(2);
});
