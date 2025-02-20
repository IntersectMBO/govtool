import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import { expect } from "@playwright/test";
import { test } from "@fixtures/walletExtension";

test.beforeEach(async () => {
  await setAllureEpic("7. Proposal submission");
  await skipIfNotHardFork();
});

test("7A. Should open wallet connection popup, when propose a governance action in disconnected state.", async ({
  page,
}) => {
  await page.goto("/");
  await page.getByTestId("propose-a-governance-action-button").click();
  await expect(page.getByTestId("connect-your-wallet-modal")).toBeVisible();
});
