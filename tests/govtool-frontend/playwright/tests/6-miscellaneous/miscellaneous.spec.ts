import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { isMobile, openDrawer } from "@helpers/mobile";
import { expect } from "@playwright/test";
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

test("6D Should open Sanchonet docs in a new tab when clicking `Learn More` on dashboards in disconnected state.", async ({
  page,
  context,
}) => {
  await page.goto("/");

  const [delegationLearnMorepage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("delegate-learn-more-button").click(),
  ]);

  await expect(delegationLearnMorepage).toHaveURL(
    `${environments.docsUrl}/faqs/ways-to-use-your-voting-power`
  );

  const [registerLearnMorepage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("register-learn-more-button").click(),
  ]);

  await expect(registerLearnMorepage).toHaveURL(
    `${environments.docsUrl}/faqs/what-does-it-mean-to-register-as-a-drep`
  );

  const [directVoterLearnMorepage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("lear-more-about-sole-voter-button").click(),
  ]);

  await expect(directVoterLearnMorepage).toHaveURL(`${environments.docsUrl}`);

  const [proposed_GA_VoterLearnMorepage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByRole("button", { name: "Learn more" }).nth(3).click(), // BUG missing test id
  ]);

  await expect(proposed_GA_VoterLearnMorepage).toHaveURL(
    `${environments.docsUrl}`
  );
});
