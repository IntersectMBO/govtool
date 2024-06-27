import {
  ABSTAIN_VOTE_DOC_URL,
  DELEGATION_DOC_URL,
  DIRECT_VOTER_DOC_URL,
  GOVERNANCE_ACTION_DOC_URL,
  PROPOSE_GOVERNANCE_ACTION_DOC_URL,
  REGISTER_DREP_DOC_URL,
  SIGNAL_NO_CONFIDENCE_VOTE_DOC_URL,
} from "@constants/docsUrl";
import { user01Wallet } from "@constants/staticWallets";
import { createTempUserAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { ShelleyWallet } from "@helpers/crypto";
import { createNewPageWithWallet } from "@helpers/page";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import EditDRepPage from "@pages/editDRepPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import { expect } from "@playwright/test";

test.beforeEach(async () => {
  await setAllureEpic("6. Miscellaneous");
});

test.describe("Logged in user", () => {
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
    await expect(delegationLearnMorepage).toHaveURL(DELEGATION_DOC_URL);

    const [registerLearnMorepage] = await Promise.all([
      context.waitForEvent("page"),
      page.getByTestId("register-learn-more-button").click(),
    ]);
    await expect(registerLearnMorepage).toHaveURL(REGISTER_DREP_DOC_URL);

    const [directVoterLearnMorepage] = await Promise.all([
      context.waitForEvent("page"),
      page.getByTestId("learn-more-button").first().click(), // BUG should be unique test id
    ]);
    await expect(directVoterLearnMorepage).toHaveURL(DIRECT_VOTER_DOC_URL);

    const [GA_LearnMorepage] = await Promise.all([
      context.waitForEvent("page"),
      page.getByTestId("learn-more-governance-actions-button").click(),
    ]);
    await expect(GA_LearnMorepage).toHaveURL(GOVERNANCE_ACTION_DOC_URL);

    const [proposed_GA_VoterLearnMorepage] = await Promise.all([
      context.waitForEvent("page"),
      page
        .locator("div")
        .filter({ hasText: /^ProposeLearn more$/ })
        .getByTestId("learn-more-button")
        .click(),
    ]); // BUG should be unique test id
    await expect(proposed_GA_VoterLearnMorepage).toHaveURL(
      PROPOSE_GOVERNANCE_ACTION_DOC_URL
    );
  });

  test("6F. Should open sanchonet docs in a new tab when clicking `info` button of abstain and signal-no-confidence card", async ({
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
    await expect(abstain_Info_Page).toHaveURL(ABSTAIN_VOTE_DOC_URL);

    const [signal_No_Confidence_Info_Page] = await Promise.all([
      context.waitForEvent("page"),
      dRepDirectoryPage.signalNoConfidenceInfoButton.click(),
    ]);
    await expect(signal_No_Confidence_Info_Page).toHaveURL(
      SIGNAL_NO_CONFIDENCE_VOTE_DOC_URL
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
});

test.describe("Temporary user", () => {
  test("6J. Should add a username.", async ({ page, browser }) => {
    const wallet = (await ShelleyWallet.generate()).json();
    const tempUserAuth = await createTempUserAuth(page, wallet);
    const userPage = await createNewPageWithWallet(browser, {
      storageState: tempUserAuth,
      wallet,
    });

    const proposalDiscussionPage = new ProposalDiscussionPage(userPage);
    await proposalDiscussionPage.goto();
    await proposalDiscussionPage.setUsername(faker.internet.userName());
  });
});
