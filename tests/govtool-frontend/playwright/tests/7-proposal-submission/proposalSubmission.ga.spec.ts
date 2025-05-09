import environments from "@constants/environments";
import { createTempUserAuth } from "@datafactory/createAuth";
import { test } from "@fixtures/proposal";
import { setAllureEpic } from "@helpers/allure";
import { createNewPageWithWallet, logWalletDetails } from "@helpers/page";
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
import { valid as mockValid, invalid as mockInvalid } from "@mock/index";
import { rewardAddressBech32 } from "@helpers/shellyWallet";
import { getWalletConfigForFaucet } from "@helpers/index";
import { faker } from "@faker-js/faker";
import { proposalSubmissionAuthFile } from "@constants/auth";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";

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
    await logWalletDetails(wallet.address);

    const tempUserAuth = await createTempUserAuth(page, wallet);

    const userPage = await createNewPageWithWallet(browser, {
      storageState: tempUserAuth,
      wallet: wallet,
    });

    const proposalDiscussionPage = new ProposalDiscussionPage(userPage);
    await proposalDiscussionPage.goto();
    await proposalDiscussionPage.verifyIdentityBtn.click();
    await proposalDiscussionPage.setUsername(mockValid.username());

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

test.describe("Proposed as a governance action", async () => {
  let proposalSubmissionPage: ProposalSubmissionPage;
  let proposalDiscussionDetailPage: ProposalDiscussionDetailsPage;
  let proposalId: number;

  test.beforeEach(async ({ browser }) => {
    const proposalSubmissionWallet =
      await walletManager.getFirstWalletByPurpose("proposalSubmissionCopy");
    await logWalletDetails(proposalSubmissionWallet.address);

    const page = await createNewPageWithWallet(browser, {
      storageState: proposalSubmissionAuthFile,
      wallet: proposalSubmissionWallet,
    });

    proposalSubmissionPage = new ProposalSubmissionPage(page);
    await proposalSubmissionPage.goto();

    proposalDiscussionDetailPage = new ProposalDiscussionDetailsPage(page);

    const rewardAddress = rewardAddressBech32(
      environments.networkId,
      getWalletConfigForFaucet().stake.pkh
    );

    proposalId = await proposalSubmissionPage.createProposal(rewardAddress);
    await proposalDiscussionDetailPage.submitAsGABtn.click();
    await proposalSubmissionPage.currentPage
      .getByTestId("agree-checkbox")
      .click();
    await proposalSubmissionPage.continueBtn.click();
  });

  test.afterEach(async () => {
    // cleanup
    await proposalDiscussionDetailPage.goto(proposalId);
    await proposalDiscussionDetailPage.deleteProposal();
  });

  test.describe("Metadata anchor validation", () => {
    test("7J_1. Should accept valid metadata anchor on proposal submission", async () => {
      test.slow(); // Brute-force testing with 50 random data
      for (let i = 0; i < 50; i++) {
        await proposalSubmissionPage.metadataUrlInput.fill(mockValid.url());
        await expect(
          proposalSubmissionPage.currentPage.getByTestId("url-input-error-text")
        ).toBeHidden();
      }
    });

    test("7J_2. Should reject invalid metadata anchor on proposal submission", async () => {
      test.slow(); // Brute-force testing with 50 random data
      for (let i = 0; i < 50; i++) {
        await proposalSubmissionPage.metadataUrlInput.fill(
          mockInvalid.url(false)
        );
        await expect(
          proposalSubmissionPage.currentPage.getByTestId("url-input-error-text")
        ).toBeVisible();
      }

      const sentenceWithoutSpace = faker.lorem
        .sentence(128)
        .replace(/[\s.]/g, "");
      const metadataAnchorGreaterThan128Bytes =
        faker.internet.url({ appendSlash: true }) + sentenceWithoutSpace;

      await proposalSubmissionPage.metadataUrlInput.fill(
        metadataAnchorGreaterThan128Bytes
      );

      await expect(
        proposalSubmissionPage.currentPage.getByTestId("url-input-error-text")
      ).toBeVisible(); // BUG better to add different test id compare to invalid url testid
    });
  });

  test("7K. Should reject invalid proposal metadata", async () => {
    await proposalSubmissionPage.metadataUrlInput.fill(faker.internet.url());
    await proposalSubmissionPage.submitBtn.click();

    await expect(
      proposalSubmissionPage.currentPage.getByTestId("url-error-modal-title")
    ).toHaveText(/the url you entered cannot be found/i);
  });
});
