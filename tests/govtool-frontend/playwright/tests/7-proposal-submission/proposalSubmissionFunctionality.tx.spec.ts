import environments from "@constants/environments";
import { createTempUserAuth } from "@datafactory/createAuth";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { ShelleyWallet } from "@helpers/crypto";
import { createNewPageWithWallet } from "@helpers/page";
import { waitForTxConfirmation } from "@helpers/transaction";
import ProposalSubmissionPage from "@pages/proposalSubmissionPage";
import { Page, expect } from "@playwright/test";
import { IProposalForm, ProposalType, StaticWallet } from "@types";
import walletManager from "lib/walletManager";

test.beforeEach(async () => {
  await setAllureEpic("7. Proposal submission");
});

test.describe("Proposal submission check", () => {
  let userPage: Page;
  let wallet: StaticWallet;

  test.beforeEach(async ({ browser, page }, testInfo) => {
    wallet = await walletManager.popWallet("adaHolder");

    const tempUserAuth = await createTempUserAuth(page, wallet);

    userPage = await createNewPageWithWallet(browser, {
      storageState: tempUserAuth,
      wallet,
    });
  });

  Object.values(ProposalType).map((type: ProposalType, index) => {
    test(`7G_${index + 1}. Should open wallet connection popup, when registered with proper ${type.toLowerCase()} data`, async ({
      page,
      browser,
    }, testInfo) => {
      const proposalSubmissionPage = new ProposalSubmissionPage(userPage);

      await proposalSubmissionPage.goto();

      await userPage.getByTestId(`${type}-radio`).click();
      await proposalSubmissionPage.continueBtn.click();

      const walletAddressBech32 =
        ShelleyWallet.fromJson(wallet).rewardAddressBech32(0);

      const proposal: IProposalForm =
        proposalSubmissionPage.generateValidProposalFormFields(
          type,
          walletAddressBech32
        );
      await proposalSubmissionPage.register({ ...proposal });
      await expect(
        proposalSubmissionPage.registrationErrorModal.getByText(
          "UTxO Balance Insufficient"
        )
      ).toBeVisible();
    });
  });
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

  const proposal: IProposalForm =
    proposalSubmissionPage.generateValidProposalFormFields(ProposalType.info);

  await proposalSubmissionPage.register({ ...proposal });

  await expect(
    userPage.getByTestId("governance-action-submitted-modal")
  ).toBeVisible();

  await userPage.getByTestId("confirm-modal-button").click();

  await waitForTxConfirmation(userPage);
});
