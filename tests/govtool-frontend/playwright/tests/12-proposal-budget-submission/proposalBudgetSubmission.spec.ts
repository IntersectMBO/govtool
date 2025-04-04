import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { isMobile } from "@helpers/mobile";
import { expect } from "@playwright/test";

test.beforeEach(async ({}) => {
  await setAllureEpic("12. Proposal Budget Submission");
});

test("12A. Should restrict from creating a budget proposal in disconnect state", async ({
  page,
}) => {
  await page.goto("/");
  if (isMobile(page)) {
    await page.getByTestId("open-drawer-button").click();
  }
  await page.getByTestId("budget-discussion-link").click();
  await expect(page.getByTestId("verify-identity-button")).not.toBeVisible();
  await expect(
    page.getByTestId("propose-a-budget-discussion-button")
  ).not.toBeVisible();
});
