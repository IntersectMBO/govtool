import environments from "@constants/environments";
import { createTempUserAuth } from "@datafactory/createAuth";
import { test } from "@fixtures/proposal";
import { setAllureEpic } from "@helpers/allure";
import { createNewPageWithWallet } from "@helpers/page";
import { waitForTxConfirmation } from "@helpers/transaction";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import ProposalSubmissionPage from "@pages/proposalSubmissionPage";
import { expect } from "@playwright/test";
import {
  skipIfMainnet,
  skipIfTemporyWalletIsNotAvailable,
} from "@helpers/cardano";
import { ProposalType } from "@types";
import walletManager from "lib/walletManager";
import { valid } from "@mock/index";
import { rewardAddressBech32 } from "@helpers/shellyWallet";
import { getWalletConfigForFaucet } from "@helpers/index";

test.beforeEach(async () => {
  await setAllureEpic("7. Proposal submission");
  await skipIfMainnet();
  await skipIfTemporyWalletIsNotAvailable("proposalSubmissionWallets.json");
});

Object.values(ProposalType).forEach((proposalType, index) => {
  test(`7H_${index + 1}. Should submit a ${proposalType.toLocaleLowerCase()} proposal as governance action`, async ({
    page,
    browser,
  }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const wallet = await walletManager.popWallet("proposalSubmission");

    const tempUserAuth = await createTempUserAuth(page, wallet);

    const userPage = await createNewPageWithWallet(browser, {
      storageState: tempUserAuth,
      wallet: wallet,
    });

    const proposalDiscussionPage = new ProposalDiscussionPage(userPage);
    await proposalDiscussionPage.goto();
    await proposalDiscussionPage.verifyIdentityBtn.click();
    await proposalDiscussionPage.setUsername(valid.username());

    const proposalSubmissionPage = new ProposalSubmissionPage(userPage);
    await proposalSubmissionPage.proposalCreateBtn.click();
    await proposalDiscussionPage.continueBtn.click();

    const rewardAddress = rewardAddressBech32(
      environments.networkId,
      getWalletConfigForFaucet().stake.pkh
    );

    await proposalSubmissionPage.createProposal(rewardAddress, proposalType);

    await userPage.getByTestId("submit-as-GA-button").click();

    await userPage.getByTestId("agree-checkbox").click();
    proposalSubmissionPage.continueBtn.click();

    await proposalSubmissionPage.fillUpValidMetadata();

    await expect(userPage.getByTestId("ga-submitted-modal-title")).toHaveText(
      /governance action submitted!/i,
      {
        timeout: 60_000,
      }
    );

    await waitForTxConfirmation(userPage);
  });
});
