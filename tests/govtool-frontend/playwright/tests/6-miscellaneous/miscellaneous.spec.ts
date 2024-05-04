import { expect, test } from "@playwright/test";
import environments from "lib/constants/environments";

test("6C. Navigation within the dApp @smoke @fast", async ({
  page,
  context,
}) => {
  await page.goto("/");

  await page.getByTestId("governance-actions-link").click();
  await expect(page).toHaveURL(/\/governance_actions/);

  const [guidesPage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("guides-link").click(),
  ]);

  await expect(guidesPage).toHaveURL(
    `${environments.docsUrl}/about/what-is-sanchonet-govtool`
  );

  const [faqsPage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("faqs-link").click(),
  ]);

  await expect(faqsPage).toHaveURL(`${environments.docsUrl}/faqs`);

  await page.getByTestId("dashboard-link").click();
  expect(page.url()).toEqual(`${environments.frontendUrl}/`);
});
