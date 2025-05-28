import { setAllureEpic } from "@helpers/allure";
import { expect } from "@playwright/test";
import { test } from "@fixtures/walletExtension";

test.beforeEach(async () => {
  await setAllureEpic("3. DRep registration");
});

test("3C. Should open wallet connection popup on DRep registration in disconnected state", async ({
  page,
}) => {
  await page.goto("/");

  await page.getByTestId("home-card-become-a-drep").click();
  await expect(page.getByTestId("connect-your-wallet-modal")).toBeVisible();
});
