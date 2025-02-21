import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import { expect } from "@playwright/test";
import { test } from "@fixtures/walletExtension";

test.beforeEach(async () => {
  await setAllureEpic("3. DRep registration");
  await skipIfNotHardFork();
});

test("3C. Should open wallet connection popup on DRep registration in disconnected state", async ({
  page,
}) => {
  await page.goto("/");

  await page.getByTestId("register-connect-wallet-button").click();
  await expect(page.getByTestId("connect-your-wallet-modal")).toBeVisible();
});
