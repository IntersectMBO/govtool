import { expect, test } from "@playwright/test";

test("2C. Verify DRep Behavior in Disconnected State @smoke @fast", async ({
  page,
}) => {
  await page.goto("/");

  await page.getByTestId("delegate-connect-wallet-button").click();
  await page
    .locator('[data-testid$="-connect-to-delegate-button"]')
    .first()
    .click();
  await expect(page.getByTestId("connect-your-wallet-modal")).toBeVisible();
});
