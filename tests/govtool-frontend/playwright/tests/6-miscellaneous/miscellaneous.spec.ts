import {
  BOOTSTRAP_DOC_URL,
  DELEGATION_DOC_URL,
  DIRECT_VOTER_DOC_URL,
  FAQS_DOC_URL,
  GUIDES_DOC_URL,
  HELP_DOC_URL,
  PRIVACY_POLICY,
  REGISTER_DREP_DOC_URL,
  TERMS_AND_CONDITIONS,
} from "@constants/docsUrl";
import { faker } from "@faker-js/faker";
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
    page.getByTestId("d-rep-learn-more-button").click(),
  ]);
  await expect(registerLearnMorepage).toHaveURL(REGISTER_DREP_DOC_URL);

  const [directVoterLearnMorepage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("direct-voter-learn-more-button").click(),
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
    page.getByTestId("terms-of-use-footer-link").click(),
  ]);
  await expect(termsAndConditions).toHaveURL(TERMS_AND_CONDITIONS);

  const [helpUrl] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("help-footer-button").click(),
  ]);
  await expect(helpUrl).toHaveURL(HELP_DOC_URL);
});

test.describe("User Snap", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/");
    await page.waitForTimeout(2_000); // wait until page load properly

    await page.getByTestId("feedback-footer-button").click();
  });

  test("6N. Should open feedback modal", async ({ page }) => {
    await expect(page.getByLabel("Usersnap widget")).toBeVisible();
    await expect(
      page.getByRole("button", {
        name: "Report an issue Something",
      })
    ).toBeVisible();
    await expect(
      page.getByRole("button", {
        name: "Idea or new feature Let us",
      })
    ).toBeVisible();
  });

  test("6O. Should verify a bug report form", async ({ page }) => {
    await page
      .getByRole("button", {
        name: "Report an issue Something",
      })
      .click();

    await expect(
      page.getByRole("heading", { name: "Report a bug" })
    ).toBeVisible();
    await expect(page.getByPlaceholder("Your feedback")).toBeVisible();
    await expect(page.getByText("Drag & drop or Browse")).toBeVisible();
    await expect(page.getByLabel("Take screenshot")).toBeVisible();
    await expect(page.getByLabel("Record")).toBeVisible();
    await expect(page.getByRole("button", { name: "Submit" })).toBeVisible();
  });

  test("6P. Should verify feature form", async ({ page }) => {
    await page
      .getByRole("button", {
        name: "Idea or new feature Let us",
      })
      .click();

    await expect(
      page.getByRole("heading", { name: "Idea or new feature" })
    ).toBeVisible();
    await expect(
      page.getByPlaceholder("Example: New navigation")
    ).toBeVisible();
    await expect(
      page.getByPlaceholder("Example: New navigation")
    ).toBeVisible();
    await expect(page.getByLabel("Any additional details")).toBeVisible();
    await expect(page.getByText("Drag & drop or Browse")).toBeVisible();
    await expect(
      page.getByLabel("Please summarize your idea or")
    ).toBeVisible();
    await expect(page.getByLabel("Take screenshot")).toBeVisible();
    await expect(page.getByLabel("Record")).toBeVisible();
    await expect(page.getByRole("button", { name: "Submit" })).toBeVisible();
  });

  test.describe("Feedback Tests", () => {
    const attachmentInputSelector = "input[type=file]";
    const feedbackApiUrl =
      "https://widget.usersnap.com/api/widget/xhrrpc?submit_feedback";
    const mockAttachmentPath = "./lib/_mock/mockAttachment.png";

    test("6Q. Should report an issue", async ({ page }) => {
      // Intercept Usersnap submit API
      await page.route(feedbackApiUrl, async (route) =>
        route.fulfill({
          status: 403,
          body: JSON.stringify({ error: "Blocked by test" }),
        })
      );

      await page
        .getByRole("button", {
          name: "Report an issue Something",
        })
        .click();

      await page
        .getByPlaceholder("Your feedback")
        .fill(faker.lorem.paragraph(2));
      await page.setInputFiles(attachmentInputSelector, [mockAttachmentPath]);

      await page.getByRole("button", { name: "Submit" }).click();

      await expect(page.getByText("Feedback was not submitted,")).toBeVisible();
    });

    test("6R. Should submit an idea or new feature", async ({ page }) => {
      // Intercept Usersnap submit API
      await page.route(feedbackApiUrl, async (route) =>
        route.fulfill({
          status: 403,
          body: JSON.stringify({ error: "Blocked by test" }),
        })
      );

      await page
        .getByRole("button", {
          name: "Idea or new feature Let us",
        })
        .click();

      await page
        .getByPlaceholder("Example: New navigation")
        .fill(faker.lorem.words(4));
      await page
        .getByLabel("Please summarize your idea or")
        .fill(faker.lorem.paragraph(2));
      await page
        .getByLabel("Any additional details")
        .fill(faker.lorem.paragraph(2));
      await page.setInputFiles(attachmentInputSelector, [mockAttachmentPath]);

      await page.getByRole("button", { name: "Submit" }).click();

      await expect(page.getByText("Feedback was not submitted,")).toBeVisible();
    });
  });
});

test("6S. Should Warn users that they are in bootstrapping phase via banner", async ({
  page,
  context,
}) => {
  await page.route("**/epoch/params", async (route) => {
    // Fetch the original response from the server
    const response = await route.fetch();
    const json = await response.json();

    // update protocol major version
    json["protocol_major"] = 9;
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify(json),
    });
  });

  const responsePromise = page.waitForResponse("**/epoch/params");
  await page.goto("/");

  await responsePromise;

  await expect(page.getByTestId("system-bootstrapping-warning")).toBeVisible();

  const [bootstrap] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("system-bootstrapping-warning-link").click(),
  ]);
  await expect(bootstrap).toHaveURL(BOOTSTRAP_DOC_URL);
});

test("6T. Should display proper network name", async ({ page }) => {
  await page.route("**/network/metrics", async (route) => {
    // Fetch the original response from the server
    const response = await route.fetch();
    const json = await response.json();

    const networkNames = ["sanchonet", "preview"];
    // update network name
    json["networkName"] =
      networkNames[Math.floor(Math.random() * networkNames.length)];
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify(json),
    });
  });
  const responsePromise = page.waitForResponse("**/network/metrics");
  await page.goto("/");

  const response = await responsePromise;
  const responseBody = await response.json();

  await expect(page.getByTestId("system-network-name")).toHaveText(
    new RegExp(responseBody["networkName"], "i")
  );
});
