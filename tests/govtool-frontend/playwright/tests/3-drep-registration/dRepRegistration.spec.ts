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

  await page.getByLabel("Become a DRep. DReps are").click(); //BUG missing test id
  await expect(page.getByTestId("connect-your-wallet-modal")).toBeVisible();
});
