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
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { isMobile, openDrawer } from "@helpers/mobile";
import { expect, Page } from "@playwright/test";
import environments from "lib/constants/environments";

test.beforeEach(async () => {
  await setAllureEpic("6. Miscellaneous");
});

test("6C. Navigation within the dApp", async ({ page, context }) => {
  await page.goto("/");

  const navbarLinks = [
    { testId: "dashboard-link", url: `${environments.frontendUrl}/` },
    { testId: "drep-directory-link", urlPattern: /\/drep_directory/ },
    { testId: "budget-discussion-link", urlPattern: /\/budget_discussion/ },
    {
      testId: "proposed-governance-actions-link",
      urlPattern: /\/proposal_discussion/,
      isDropdownContent: true,
    },
    {
      testId: "governance-actions-link",
      urlPattern: /\/governance_actions/,
      isDropdownContent: true,
    },
    {
      testId: "governance-actions-outcomes-link",
      urlPattern: /\/outcomes/,
      isDropdownContent: true,
    },
  ];

  for (const link of navbarLinks) {
    if (isMobile(page)) {
      await openDrawer(page);
    }

    if (!isMobile(page) && !!link.isDropdownContent) {
      await page.getByTestId("governance-actions").click();
    }
    await page.getByTestId(link.testId).click();

    if (link.url) {
      expect(page.url()).toEqual(link.url);
    } else {
      await expect(page).toHaveURL(link.urlPattern);
    }
  }

  const externalLinks = [
    { testId: "guides-link", url: GUIDES_DOC_URL },
    { testId: "faqs-link", url: FAQS_DOC_URL },
  ];

  for (const link of externalLinks) {
    if (isMobile(page)) {
      await openDrawer(page);
    }
    const [newPage] = await Promise.all([
      context.waitForEvent("page"),
      page.getByTestId(link.testId).click(),
    ]);
    await expect(newPage).toHaveURL(link.url);
  }
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

test("6O. Should display proper network name", async ({ page }) => {
  await page.route("**/network/info", async (route) => {
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
  const responsePromise = page.waitForResponse("**/network/info");
  await page.goto("/");

  const response = await responsePromise;
  const responseBody = await response.json();

  await expect(page.getByTestId("system-network-name")).toHaveText(
    new RegExp(responseBody["networkName"], "i")
  );
});
