import environments from "@constants/environments";
import { adaHolder01Wallet, user01Wallet } from "@constants/staticWallets";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import EditDRepPage from "@pages/editDRepPage";
import { expect } from "@playwright/test";

test.beforeEach(async () => {
  await setAllureEpic("6. Miscellaneous");
});

test.use({
  storageState: ".auth/user01.json",
  wallet: user01Wallet,
});

test("6E. Should open Sanchonet docs in a new tab when clicking `Learn More` on dashboards in connected state.", async ({
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
    page.getByTestId("learn-more-button").first().click(), // BUG should be unique test id
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
  ]); // BUG should be unique test id

  await expect(proposed_GA_VoterLearnMorepage).toHaveURL(
    `${environments.docsUrl}/faqs/what-is-a-governance-action`
  );
});

test("6F. should open sanchonet docs in a new tab when clicking `info` button of abstain and signal-no-confidence card", async ({
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

test("6G. Should restrict edit dRep for non dRep", async ({ page }) => {
  const editDrepPage = new EditDRepPage(page);
  await editDrepPage.goto();

  await page.waitForTimeout(2_000);
  await expect(editDrepPage.nameInput).not.toBeVisible();
});

test("6I. Should prompt for a username after clicking on proposal discussion link if username is not set", async ({
  page,
}) => {
  await page.goto("/");
  await page.getByTestId("proposal-discussion-link").click();

  await expect(
    page.getByText(
      "Hey, setup your usernameUsername cannot be changed in the Future. Some subtext"
    )
  ).toBeVisible(); //BUG Add modal testid instead should be username-modal

  await expect(page.getByLabel("Username *")).toBeVisible(); // BUG use testid instead
});

test.describe("Add username", () => {
  test.use({
    storageState: ".auth/adaHolder01.json",
    wallet: adaHolder01Wallet,
  });

  test("6J. Should add a username.", async ({ page }) => {
    const username = faker.person.firstName();

    await page.goto("/");
    await page.getByTestId("proposal-discussion-link").click();

    await page.getByLabel("Username *").fill(username); //BUG used testid instead

    await page
      .getByRole("button", { name: "Proceed with this username" })
      .click(); //BUG used testid instead

    await page
      .getByRole("button", { name: "Proceed with this username" })
      .click();

    await page.getByRole("button", { name: "Close" }).click();

    await expect(
      page.getByRole("button", { name: "Propose a Governance Action" })
    ).toBeVisible();

    await expect(page.getByPlaceholder("Search...")).toBeVisible();
  });
});
