import {
  DELEGATION_DOC_URL,
  DIRECT_VOTER_DOC_URL,
  FAQS_DOC_URL,
  GUIDES_DOC_URL,
  HELP_DOC_URL,
  PRIVACY_POLICY,
  REGISTER_DREP_DOC_URL,
  TERMS_AND_CONDITIONS,
} from "@constants/docsUrl";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { isMobile, openDrawer } from "@helpers/mobile";
import UserSnapPage from "@pages/userSnapPage";
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

  await expect(guidesPage).toHaveURL(GUIDES_DOC_URL);

  if (isMobile(page)) {
    await openDrawer(page);
  }
  const [faqsPage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("faqs-link").click(),
  ]);

  await expect(faqsPage).toHaveURL(FAQS_DOC_URL);

  if (isMobile(page)) {
    await openDrawer(page);
  }
  await page.getByTestId("dashboard-link").click();
  expect(page.url()).toEqual(`${environments.frontendUrl}/`);
});

test("6D. Should open Sanchonet docs in a new tab when clicking `Learn More` on dashboards in disconnected state.", async ({
  page,
  context,
}) => {
  await page.goto("/");

  const [delegationLearnMorepage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("delegate-learn-more-button").click(),
  ]);
  await expect(delegationLearnMorepage).toHaveURL(DELEGATION_DOC_URL);

  const [registerLearnMorepage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("register-learn-more-button").click(),
  ]);
  await expect(registerLearnMorepage).toHaveURL(REGISTER_DREP_DOC_URL);

  const [directVoterLearnMorepage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("lear-more-about-sole-voter-button").click(),
  ]);
  await expect(directVoterLearnMorepage).toHaveURL(DIRECT_VOTER_DOC_URL);
});

test("6M. Should navigate between footer links", async ({ page, context }) => {
  await page.goto("/");

  const [privacyPolicy] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("privacy-policy-footer-link").click(),
  ]);
  await expect(privacyPolicy).toHaveURL(PRIVACY_POLICY);

  const [termsAndConditions] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("term-of-service-footer-link").click(),
  ]);
  await expect(termsAndConditions).toHaveURL(TERMS_AND_CONDITIONS);

  const [helpUrl] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("help-footer-button").click(),
  ]);
  await expect(helpUrl).toHaveURL(HELP_DOC_URL);
});

test("6N. Should open feedback modal", async ({ page }) => {
  const userSnapPage = new UserSnapPage(page);
  await userSnapPage.goto();

  await expect(userSnapPage.userSnapModal).toBeVisible();
  await expect(userSnapPage.reportABugBtn).toBeVisible();
  await expect(userSnapPage.ideaOrNewFeatureBtn).toBeVisible();
});

test("6O. Should verify a bug report form", async ({ page }) => {
  const userSnapPage = new UserSnapPage(page);
  await userSnapPage.goto();

  await userSnapPage.reportABugBtn.click();

  await expect(
    page.getByRole("heading", { name: "Report a bug" })
  ).toBeVisible();
  await expect(userSnapPage.feedbackInput).toBeVisible();
  await expect(userSnapPage.addAttachmentBtn).toBeVisible();
  await expect(userSnapPage.emailInput).toBeVisible();
  await expect(userSnapPage.takeScreenshotBtn).toBeVisible();
  await expect(userSnapPage.recordBtn).toBeVisible();
  await expect(userSnapPage.submitBtn).toBeVisible();
});

test("6P. Should verify feature form", async ({ page }) => {
  const userSnapPage = new UserSnapPage(page);
  await userSnapPage.goto();

  await userSnapPage.ideaOrNewFeatureBtn.click();

  await expect(
    page.getByRole("heading", { name: "Idea or new feature" })
  ).toBeVisible();
  await expect(userSnapPage.ideaOrNewFeatureInput).toBeVisible();
  await expect(userSnapPage.summarizeIdeaInput).toBeVisible();
  await expect(userSnapPage.additionalDetailsInput).toBeVisible();
  await expect(userSnapPage.addAttachmentBtn).toBeVisible();
  await expect(userSnapPage.emailInput).toBeVisible();
  await expect(userSnapPage.takeScreenshotBtn).toBeVisible();
  await expect(userSnapPage.recordBtn).toBeVisible();
  await expect(userSnapPage.submitBtn).toBeVisible();
});

test("6Q. Should report an issue ", async ({ page }) => {
  // intercept usersnap submit api
  await page.route(
    "https://widget.usersnap.com/api/widget/xhrrpc?submit_feedback",
    async (route) =>
      route.fulfill({
        status: 200,
      })
  );

  const userSnapPage = new UserSnapPage(page);
  await userSnapPage.goto();

  await userSnapPage.reportABugBtn.click();

  await userSnapPage.fillupBugForm();

  await userSnapPage.submitBtn.click();

  await expect(page.getByText("Feedback was not submitted,")).toBeVisible();
});

test("6R. Should submit an idea or new feature", async ({ page }) => {
  // intercept usersnap submit api
  await page.route(
    "https://widget.usersnap.com/api/widget/xhrrpc?submit_feedback",
    async (route) =>
      route.fulfill({
        status: 200,
      })
  );

  const userSnapPage = new UserSnapPage(page);
  await userSnapPage.goto();

  await userSnapPage.ideaOrNewFeatureBtn.click();

  await userSnapPage.fillupFeatureForm();

  await userSnapPage.submitBtn.click();

  await expect(page.getByText("Feedback was not submitted,")).toBeVisible();
});
