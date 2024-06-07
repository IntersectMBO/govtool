import { dRep01Wallet, user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { isMobile, openDrawer } from "@helpers/mobile";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import EditDRepPage from "@pages/editDRepPage";
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
    page.getByRole("button", { name: "Learn more" }).nth(3).click(),
  ]);

  await expect(proposed_GA_VoterLearnMorepage).toHaveURL(
    `${environments.docsUrl}`
  );
});

test.describe("wallet connect state", () => {
  test.use({
    storageState: ".auth/user01.json",
    wallet: user01Wallet,
  });

  test("6E Should open Sanchonet docs in a new tab when clicking `Learn More` on dashboards in connected state.", async ({
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
      page.getByTestId("learn-more-button").first().click(),
    ]);

    await expect(directVoterLearnMorepage).toHaveURL(
      `${environments.docsUrl}/faqs/what-does-it-mean-to-register-as-a-drep`
    );

    const [GA_LearnMorepage] = await Promise.all([
      context.waitForEvent("page"),
      page.getByTestId("learn-more-governance-actions-button").click(),
    ]);

    await expect(GA_LearnMorepage).toHaveURL("https://sancho.network/actions/");

    const [proposed_GA_VoterLearnMorepage] = await Promise.all([
      context.waitForEvent("page"),
      page
        .locator("div")
        .filter({ hasText: /^ProposeLearn more$/ })
        .getByTestId("learn-more-button")
        .click(),
    ]);

    await expect(proposed_GA_VoterLearnMorepage).toHaveURL(
      `${environments.docsUrl}/faqs/what-is-a-governance-action`
    );
  });

  test("6F should open sanchonet docs in a new tab when clicking `info` button of abstain and signal-no-confidence card", async ({
    page,
    context,
  }) => {
    const dRepDirectoryPage = new DRepDirectoryPage(page);
    await dRepDirectoryPage.goto();

    await dRepDirectoryPage.automaticDelegationOptionsDropdown.click();

    const [abstain_Info_Page] = await Promise.all([
      context.waitForEvent("page"),
      dRepDirectoryPage.abstainInfoButton.click(),
    ]);

    await expect(abstain_Info_Page).toHaveURL(`${environments.docsUrl}`);

    const [signal_No_Confidence_Info_Page] = await Promise.all([
      context.waitForEvent("page"),
      dRepDirectoryPage.signalNoConfidenceInfoButton.click(),
    ]);

    await expect(signal_No_Confidence_Info_Page).toHaveURL(
      `${environments.docsUrl}`
    );
  });

  test("6G Should restrict edit dRep for non dRep", async ({ page }) => {
    const editDrepPage = new EditDRepPage(page);
    await editDrepPage.goto();

    await page.waitForTimeout(2_000);
    await expect(editDrepPage.nameInput).not.toBeVisible();
  });
});

test.describe("Registration Restriction", () => {
  test.use({
    storageState: ".auth/dRep01.json",
    wallet: dRep01Wallet,
  });

  test("6H Should restrict dRep registration for dRep", async ({ page }) => {
    await page.goto(`${environments.frontendUrl}/register_drep`);

    await page.waitForTimeout(2_000);

    await expect(page.getByTestId("name-input")).not.toBeVisible();
  });
});
