import { setAllureEpic } from "@helpers/allure";
import { isMobile, openDrawer } from "@helpers/mobile";
import { expect, test } from "@playwright/test";
import environments from "lib/constants/environments";

test.beforeEach(async () => {
  await setAllureEpic("6. Miscellaneous");
});

test("6C. Navigation within the dApp", async ({ page, context }) => {
  await page.goto("/");

  if (isMobile(page)) {
    await openDrawer(page);
  }
  await page.getByTestId("governance-actions-link").click();
  await expect(page).toHaveURL(/\/governance_actions/);

  if (isMobile(page)) {
    await openDrawer(page);
  }
  const [guidesPage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("guides-link").click(),
  ]);

  await expect(guidesPage).toHaveURL(
    `${environments.docsUrl}/about/what-is-sanchonet-govtool`
  );

  if (isMobile(page)) {
    await openDrawer(page);
  }
  const [faqsPage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("faqs-link").click(),
  ]);

  await expect(faqsPage).toHaveURL(`${environments.docsUrl}/faqs`);

  if (isMobile(page)) {
    await openDrawer(page);
  }
  await page.getByTestId("dashboard-link").click();
  expect(page.url()).toEqual(`${environments.frontendUrl}/`);
});
