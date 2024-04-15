import { test, expect } from "@playwright/test";

test("3C. Should open wallet connection popup, when Register as DRep from wallet unconnected state @smoke @fast", async ({
  page,
}) => {
  await page.goto("/");

  await page.getByTestId("register-connect-wallet-button").click();
  await expect(
    page.getByTestId("connect-your-wallet-modal").nth(1),
  ).toBeVisible();
});
