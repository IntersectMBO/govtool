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
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import { ShelleyWallet } from "@helpers/crypto";
import { createNewPageWithWallet } from "@helpers/page";
import { invalid as mockInvalid, valid as mockValid } from "@mock/index";
import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import EditDRepPage from "@pages/editDRepPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import { Page, expect } from "@playwright/test";

test.beforeEach(async () => {
  await setAllureEpic("6. Miscellaneous");
  await skipIfNotHardFork();
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
      page.getByTestId("d-rep-learn-more-button").click(),
    ]);
    await expect(registerLearnMorepage).toHaveURL(REGISTER_DREP_DOC_URL);

    const [directVoterLearnMorepage] = await Promise.all([
      context.waitForEvent("page"),
      page.getByTestId("direct-voter-learn-more-button").first().click(),
    ]);
    await expect(directVoterLearnMorepage).toHaveURL(DIRECT_VOTER_DOC_URL);

    const [GA_LearnMorepage] = await Promise.all([
      context.waitForEvent("page"),
      page.getByTestId("list-gov-actions-learn-more-button").click(),
    ]);
    await expect(GA_LearnMorepage).toHaveURL(GOVERNANCE_ACTION_DOC_URL);

    const [proposed_GA_VoterLearnMorepage] = await Promise.all([
      context.waitForEvent("page"),
      page.getByTestId("propose-gov-action-learn-more-button").click(),
    ]);
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
    await page.getByTestId("verify-identity-button").click();

    await expect(page.getByTestId("setup-username-modal")).toBeVisible();
    await expect(page.getByTestId("username-input")).toBeVisible();
  });
});

test.describe("Temporary user", () => {
  let userPage: Page;
  let proposalDiscussionPage: ProposalDiscussionPage;

  test.beforeEach(async ({ page, browser }) => {
    const wallet = (await ShelleyWallet.generate()).json();
    const tempUserAuth = await createTempUserAuth(page, wallet);
    userPage = await createNewPageWithWallet(browser, {
      storageState: tempUserAuth,
      wallet,
    });

    proposalDiscussionPage = new ProposalDiscussionPage(userPage);
    await proposalDiscussionPage.goto();
    await proposalDiscussionPage.verifyIdentityBtn.click();
  });

  test("6J. Should add a username.", async () => {
    await proposalDiscussionPage.setUsername(mockValid.username());
  });

  test("6K. Should accept valid username.", async () => {
    for (let i = 0; i < 100; i++) {
      await userPage.getByTestId("username-input").fill(mockValid.username());

      await expect(
        userPage.getByTestId("username-error-text")
      ).not.toBeVisible();
      await expect(userPage.getByTestId("proceed-button")).toBeEnabled();
    }
  });

  test("6L. Should reject invalid username.", async () => {
    for (let i = 0; i < 100; i++) {
      await userPage.getByTestId("username-input").fill(mockInvalid.username());

      await expect(userPage.getByTestId("username-error-text")).toBeVisible();
      await expect(userPage.getByTestId("proceed-button")).toBeDisabled();
    }
  });
});
