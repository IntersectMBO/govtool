import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { expect } from "@playwright/test";

test.use({ walletName: "user01", walletFundsAda: 0 });

test.beforeEach(async () => {
  await setAllureEpic("5. Proposal functionality");
});

test("5J. Should hide retirement option for non-registered DRep", async ({
  page,
}) => {
  await page.goto("/");
  await expect(page.getByTestId("retire-button")).not.toBeVisible();
});
