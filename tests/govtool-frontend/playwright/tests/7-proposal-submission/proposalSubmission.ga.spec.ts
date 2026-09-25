import environments from "@constants/environments";
import { test } from "@fixtures/proposal";
import { setAllureEpic } from "@helpers/allure";
import { createNewPageWithWallet, logWalletDetails } from "@helpers/page";
import { waitForTxConfirmation } from "@helpers/transaction";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import ProposalSubmissionPage from "@pages/proposalSubmissionPage";
import { expect, Page } from "@playwright/test";
import { skipIfMainnet, skipIfScheduledWorkflow } from "@helpers/cardano";
import { valid as mockValid, invalid as mockInvalid } from "@mock/index";
import { getProposalType } from "@helpers/index";
import { faker } from "@faker-js/faker";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import kuberService from "@services/kuberService";
import { ensureFunded, testWallet, TestWallet } from "lib/wallet/testWallets";

test.beforeEach(async () => {
  await setAllureEpic("7. Proposal submission");
  await skipIfMainnet();
});

/** The named wallet, funded with the governance action deposit and fees. */
async function proposalSubmissionWallet(name: string): Promise<TestWallet> {
  const { govActionDeposit } = await kuberService.queryProtocolParams();
  const wallet = await testWallet(name);
  await ensureFunded(wallet, govActionDeposit / 1_000_000 + 22);
  return wallet;
}

async function setUsernameIfNeeded(page: Page) {
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();
  await proposalDiscussionPage.verifyIdentityBtn.click();

  try {
    await expect(page.getByTestId("username-input")).toBeVisible({
      timeout: 10_000,
    });
    await proposalDiscussionPage.setUsername(mockValid.username());
  } catch (error) {
    // Ignore error if username is already set
    console.log("Username is already set");
  }
}

getProposalType().forEach((proposalType, index) => {
  test(`7H_${index + 1}. Should submit a ${proposalType.toLocaleLowerCase()} proposal as governance action`, async ({
    browser,
  }, testInfo) => {
    await skipIfScheduledWorkflow();
    test.setTimeout(testInfo.timeout + 2 * environments.txTimeOut);

    const wallet = await proposalSubmissionWallet(`7H_${index + 1}:proposer`);

    await logWalletDetails(wallet.address);

    const userPage = await createNewPageWithWallet(browser, { wallet });

    await setUsernameIfNeeded(userPage);
    const proposalDiscussionPage = new ProposalDiscussionPage(userPage);

    const proposalSubmissionPage = new ProposalSubmissionPage(userPage);
    await proposalSubmissionPage.proposalCreateBtn.click();
    await proposalDiscussionPage.continueBtn.click();

    await proposalSubmissionPage.createProposal(
      wallet.stakeAddress,
      proposalType
    );

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

  test.beforeEach(async ({ browser }, testInfo) => {
    test.setTimeout(testInfo.timeout + environments.txTimeOut);

    const wallet = await proposalSubmissionWallet("7:gaProposer");
    await logWalletDetails(wallet.address);

    const page = await createNewPageWithWallet(browser, { wallet });
    await setUsernameIfNeeded(page);

    proposalSubmissionPage = new ProposalSubmissionPage(page);
    await proposalSubmissionPage.goto();

    proposalDiscussionDetailPage = new ProposalDiscussionDetailsPage(page);

    proposalId = await proposalSubmissionPage.createProposal(
      wallet.stakeAddress
    );
    await proposalDiscussionDetailPage.submitAsGABtn.click();
    await proposalSubmissionPage.currentPage
      .getByTestId("agree-checkbox")
      .click();
    await proposalSubmissionPage.continueBtn.click();
  });

  test.afterEach(async () => {
    await skipIfMainnet();
    // cleanup
    await proposalDiscussionDetailPage.goto(proposalId);

    const isVerifyIdentityBtnVisible =
      await proposalDiscussionDetailPage.verifyIdentityBtn.isVisible();

    if (isVerifyIdentityBtnVisible) {
      await proposalDiscussionDetailPage.verifyIdentityBtn.click();
    }

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

  test("7P. Should navigate to the edit proposal page when 'goto data edit screen' is selected if data does not match the anchor URL", async () => {
    const invalidMetadataAnchorUrl = "https://www.google.com";

    await proposalSubmissionPage.metadataUrlInput.fill(
      invalidMetadataAnchorUrl
    );
    await proposalSubmissionPage.submitBtn.click();

    await expect(
      proposalSubmissionPage.currentPage.getByTestId("data-not-match-modal")
    ).toBeVisible();
    await expect(
      proposalSubmissionPage.currentPage.getByTestId(
        "data-not-match-modal-go-to-data-button"
      )
    ).toBeVisible();

    await proposalSubmissionPage.currentPage
      .getByTestId("data-not-match-modal-go-to-data-button")
      .click();

    await expect(
      proposalSubmissionPage.currentPage.getByTestId("governance-action-type")
    ).toBeVisible();
    await expect(
      proposalSubmissionPage.currentPage.getByTestId("title-input")
    ).toBeVisible();
    await expect(
      proposalSubmissionPage.currentPage.getByTestId("abstract-input")
    ).toBeVisible();
    await expect(
      proposalSubmissionPage.currentPage.getByTestId("motivation-input")
    ).toBeVisible();
    await expect(
      proposalSubmissionPage.currentPage.getByTestId("rationale-input")
    ).toBeVisible();
  });
});
