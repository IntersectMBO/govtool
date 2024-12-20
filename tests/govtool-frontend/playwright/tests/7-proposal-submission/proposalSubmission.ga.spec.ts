import environments from "@constants/environments";
import { createTempUserAuth } from "@datafactory/createAuth";
import { test } from "@fixtures/proposal";
import { setAllureEpic } from "@helpers/allure";
import { createNewPageWithWallet } from "@helpers/page";
import { waitForTxConfirmation } from "@helpers/transaction";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import ProposalSubmissionPage from "@pages/proposalSubmissionPage";
import { expect } from "@playwright/test";
import { skipIfNotHardFork } from "@helpers/cardano";
import { ProposalType } from "@types";
import { proposalFaucetWallet } from "@constants/proposalFaucetWallet";

test.beforeEach(async () => {
  await setAllureEpic("7. Proposal submission");
  await skipIfNotHardFork();
});

Object.values(ProposalType).forEach((proposalType, index) => {
  test(`7H_${index + 1}. Should submit a ${proposalType.toLocaleLowerCase()} proposal as governance action`, async ({
    page,
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const tempUserAuth = await createTempUserAuth(page, proposalFaucetWallet);

    const userPage = await createNewPageWithWallet(browser, {
      storageState: tempUserAuth,
      wallet: proposalFaucetWallet,
    });

    const proposalDiscussionPage = new ProposalDiscussionPage(userPage);
    await proposalDiscussionPage.goto();
    await proposalDiscussionPage.verifyIdentityBtn.click();

    const proposalSubmissionPage = new ProposalSubmissionPage(userPage);
    await proposalSubmissionPage.proposalCreateBtn.click();
    await proposalDiscussionPage.continueBtn.click();

    await proposalSubmissionPage.createProposal(
      proposalFaucetWallet,
      proposalType
    );

    await userPage.getByTestId("submit-as-GA-button").click();

    await userPage.getByTestId("agree-checkbox").click();
    proposalSubmissionPage.continueBtn.click();

    await proposalSubmissionPage.fillUpValidMetadata();

    await expect(userPage.getByTestId("ga-submitted-modal-title")).toHaveText(
      /governance action submitted!/i,
      {
        timeout: 10_000,
      }
    );

    await waitForTxConfirmation(userPage);
  });
});
