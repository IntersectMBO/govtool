import {
  adaStat,
  budgetCardanoAfrica,
  cardanoBudget,
  cardanoScan,
  CCPortal,
  cexplorer,
  DOCS_URL,
  ekklesia,
  FAQS_DOC_URL,
  GOVERNANCE_OVERVIEW_DOC_URL,
  governanceSpace,
  GUIDES_DOC_URL,
  HELP_DOC_URL,
  intersectWebsite,
  PRIVACY_POLICY,
  reachYourPeople,
  REPO_URL,
  sanchonetGovernanceExplorer,
  sixteenNinetyFour,
  sixteenNinetyFourTools,
  syncAi,
  tempo,
  TERMS_AND_CONDITIONS,
} from "@constants/docsUrl";
import { connectToCardanoWalletSection } from "@constants/index";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { isMobile, openDrawer } from "@helpers/mobile";
import { expect, Page } from "@playwright/test";
import { connect } from "http2";
import environments from "lib/constants/environments";

test.beforeEach(async () => {
  await setAllureEpic("6. Miscellaneous");
});

test("6C. Navigation within the dApp", async ({ page, context }) => {
  await page.goto("/");

  const dashboardCards = [
    { label: "Browse the DRep Directory.", urlPattern: /\/drep_directory/ },
    {
      label: "View Live Voting. See how the",
      urlPattern: /\/governance_actions/,
    },
    { label: "View Voting Outcomes. See the", urlPattern: /\/outcomes/ },
  ];

  for (const card of dashboardCards) {
    await page.getByLabel(card.label).click(); // BUG missing test id
    await expect(page).toHaveURL(card.urlPattern);
    await page.goBack();
  }

  const [guidesPage2] = await Promise.all([
    context.waitForEvent("page"),
    page.getByLabel("Read our Guides. The roadmap").click(), // BUG missing test id
  ]);

  await expect(guidesPage2).toHaveURL(GUIDES_DOC_URL);

  // Test navbar links navigation
  const navbarLinks = [
    { testId: "dashboard-link", url: `${environments.frontendUrl}/` },
    { testId: "drep-directory-link", urlPattern: /\/drep_directory/ },
    { testId: "governance-actions-link", urlPattern: /\/governance_actions/ },
    { testId: "governance-actions-outcomes-link", urlPattern: /\/outcomes/ },
  ];

  for (const link of navbarLinks) {
    if (isMobile(page)) {
      await openDrawer(page);
    }
    await page.getByTestId(link.testId).click();

    if (link.url) {
      expect(page.url()).toEqual(link.url);
    } else {
      await expect(page).toHaveURL(link.urlPattern);
    }
  }

  // Test external links in navbar
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

test("6D. Should open wallet popup when navigating from 'Connect a Cardano wallet to' section on dashboard in disconnected state", async ({
  page,
}) => {
  await page.goto("/");

  const sections = connectToCardanoWalletSection.map(
    (section) => section.testId
  );

  for (const sectionTestId of sections) {
    await page.getByTestId(sectionTestId).click();
    await expect(page.getByTestId("connect-your-wallet-modal")).toBeVisible();
    await page.getByTestId("close-modal-button").click();
  }
});

test("6G. Should navigate between other cardano governance resources", async ({
  page,
}) => {
  await page.goto("/");

  const resources = [
    { testId: "useful-link-ccPortal", url: CCPortal },
    { testId: "useful-link-intersectWebsite", url: intersectWebsite },
    { testId: "useful-link-tempo", url: tempo },
    { testId: "useful-link-1694io", url: sixteenNinetyFour },
    { testId: "useful-link-governanceSpace", url: governanceSpace },
    { testId: "useful-link-syncAi", url: syncAi },
    { testId: "useful-link-ekklesia", url: ekklesia },
    { testId: "useful-link-adaStat", url: adaStat },
    { testId: "useful-link-cexplorer", url: cexplorer },
    { testId: "useful-link-cardanoScan", url: cardanoScan },
    { testId: "useful-link-cardanoBudget", url: cardanoBudget },
    { testId: "useful-link-budgetCardanoAfrica", url: budgetCardanoAfrica },
    { testId: "useful-link-reachYourPeople", url: reachYourPeople },
    { testId: "useful-link-1694Tools", url: sixteenNinetyFourTools },
    {
      testId: "useful-link-sanchonetGovernanceExplorer",
      url: sanchonetGovernanceExplorer,
    },
  ];

  for (let resource of resources) {
    const [newPage] = await Promise.all([
      page.context().waitForEvent("page"),
      page.getByTestId(resource.testId).click(),
    ]);
    await expect(newPage).toHaveURL(resource.url);
    await newPage.close();
  }
});

test("6H. Should navigate between repository and documentation links", async ({
  page,
  context,
}) => {
  await page.goto("/");

  const [repoPage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByLabel("GitHub Repo. View the GovTool").click(), // BUG missing test id
  ]);
  await expect(repoPage).toHaveURL(REPO_URL);

  const [docsPage] = await Promise.all([
    context.waitForEvent("page"),
    page.getByLabel("Documentation. GovTool").click(), // BUG missing test id
  ]);
  await expect(docsPage).toHaveURL(DOCS_URL);

  const [governance_overview] = await Promise.all([
    context.waitForEvent("page"),
    page.getByTestId("link-to-docs").click(),
  ]);
  await expect(governance_overview).toHaveURL(GOVERNANCE_OVERVIEW_DOC_URL);
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
