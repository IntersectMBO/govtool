import { test } from "@fixtures/walletExtension";
import { dRep01Wallet } from "@constants/staticWallets";
import DelegationPage from "@pages/delegationPage";
import { expect } from "@playwright/test";

test.describe("DRep specs", () => {
  test.use({ storageState: ".auth/dRep01.json", wallet: dRep01Wallet });

  test("2D. Verify DRep Behavior in Connected State @smoke @fast", async ({
    page,
  }) => {
    const delegationPage = new DelegationPage(page);
    await delegationPage.goto();

    // Verifying delegation options
    await expect(delegationPage.delegateToDRepCard).toBeVisible();

    await delegationPage.otherOptionsBtn.click();

    await expect(delegationPage.delegateToMyselfCard).toBeVisible();
    await expect(delegationPage.signalNoConfidenceCard).toBeVisible();
    await expect(delegationPage.abstainDelegationCard).toBeVisible();

    // Verifying dRepId delegation
    await delegationPage.delegateToDRepCard.click();
    await delegationPage.nextStepBtn.click();

    await expect(delegationPage.dRepInput).toBeVisible();
    await expect(delegationPage.delegateBtn).toBeVisible();
  });
});
