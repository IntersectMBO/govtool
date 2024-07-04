import environments from "@constants/environments";
import { proposal01Wallet } from "@constants/staticWallets";
import { createTempUserAuth } from "@datafactory/createAuth";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/proposal";
import { setAllureEpic } from "@helpers/allure";
import { createNewPageWithWallet } from "@helpers/page";
import { waitForTxConfirmation } from "@helpers/transaction";
import { invalid } from "@mock/index";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import ProposalSubmissionPage from "@pages/proposalSubmissionPage";
import { expect } from "@playwright/test";
import { ProposalCreateRequest, ProposalType } from "@types";
import walletManager from "lib/walletManager";

let proposalSubmissionPage: ProposalSubmissionPage;
test.use({ storageState: ".auth/proposal01.json", wallet: proposal01Wallet });

test.beforeEach(async ({ page, proposalId }) => {
  await setAllureEpic("7. Proposal submission");

  const proposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(page);
  await proposalDiscussionDetailsPage.goto(proposalId);

  await proposalDiscussionDetailsPage.verifyIdentityBtn.click();
  await proposalDiscussionDetailsPage.submitAsGABtn.click();

  proposalSubmissionPage = new ProposalSubmissionPage(page);
  await page.click("input#submission-checkbox"); // BUG missing test id
  await page.getByRole("button", { name: "Continue" }).click();
});

test("7H. should submit a proposal", async ({ page, browser }, testInfo) => {
  test.setTimeout(testInfo.timeout + environments.txTimeOut);

  const wallet = await walletManager.popWallet("proposalSubmission");

  const tempUserAuth = await createTempUserAuth(page, wallet);

  const userPage = await createNewPageWithWallet(browser, {
    storageState: tempUserAuth,
    wallet,
  });

  const proposalSubmissionPage = new ProposalSubmissionPage(userPage);
  await proposalSubmissionPage.goto();

  await userPage.getByTestId(`${ProposalType.info}-radio`).click();
  await proposalSubmissionPage.continueBtn.click();

  const proposal: ProposalCreateRequest =
    proposalSubmissionPage.generateValidProposalFormFields(ProposalType.info);

  await proposalSubmissionPage.register({ ...proposal });

  await expect(proposalSubmissionPage.registrationSuccessModal).toBeVisible({
    timeout: 10_000,
  });

  await waitForTxConfirmation(userPage);
});

test.describe("Metadata anchor validation", () => {
  test("7J_1. Should accept valid metadata anchor on proposal submission", async ({
    page,
  }) => {
    for (let i = 0; i < 100; i++) {
      await proposalSubmissionPage.metadataUrlInput.fill(faker.internet.url());
      await expect(page.getByTestId("invalid-url-error")).toBeHidden();
    }
  });

  test("7J_2. Should reject invalid metadata anchor on proposal submission", async ({
    page,
  }) => {
    for (let i = 0; i < 100; i++) {
      await proposalSubmissionPage.metadataUrlInput.fill(invalid.url());
      await expect(page.getByTestId("invalid-url-error")).toBeVisible();
    }

    const sentenceWithoutSpace = faker.lorem
      .sentence(128)
      .replace(/[\s.]/g, "");
    const metadataAnchorGreaterThan128Bytes =
      faker.internet.url({ appendSlash: true }) + sentenceWithoutSpace;

    await proposalSubmissionPage.metadataUrlInput.fill(
      metadataAnchorGreaterThan128Bytes
    );

    await expect(page.getByTestId("invalid-url-error")).toBeVisible();
  });
});

test("7K. Should reject invalid proposal metadata", async ({ page }) => {
  await proposalSubmissionPage.metadataUrlInput.fill(invalid.url());
  await proposalSubmissionPage.submitBtn.click();

  await expect(page.getByTestId("url-error-modal-title")).toHaveText(
    /the url you entered cannot be found/i
  );
});
