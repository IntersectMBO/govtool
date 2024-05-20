import { expect, test } from "@playwright/test";

test("3C. Should open wallet connection popup on DRep registration in disconnected state", async ({
  page,
}) => {
  await page.goto("/");

  await page.getByTestId("register-connect-wallet-button").click();
  await expect(page.getByTestId("connect-your-wallet-modal")).toBeVisible();
});
