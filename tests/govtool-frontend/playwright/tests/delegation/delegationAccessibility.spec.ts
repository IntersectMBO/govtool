import { test } from "@playwright/test";
import { expect } from "@playwright/test";

test("2C. Verify DRep Behavior in Disconnected State", async ({ page }) => {
  await page.goto("/");

  await page.getByTestId("delegate-connect-wallet-button").click();
  await expect(
    page.getByTestId("connect-your-wallet-modal").nth(1),
  ).toBeVisible();
});
