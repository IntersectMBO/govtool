import environments from "@constants/environments";
import { createTempUserAuth } from "@datafactory/createAuth";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { ShelleyWallet } from "@helpers/crypto";
import { createNewPageWithWallet } from "@helpers/page";
import ProposalSubmissionPage from "@pages/proposalSubmissionPage";
import { expect } from "@playwright/test";
import { IProposalForm, ProposalType } from "@types";

test.beforeEach(async ({ browser, page }, testInfo) => {
  await setAllureEpic("7. Proposal submission");
});

test.describe("Proposal submission check", () => {
  Object.values(ProposalType).map((type: ProposalType, index) => {
    test(`7G.${index + 1}: Should open wallet connection popup, when registered with proper ${type.toLowerCase()} data`, async ({
      page,
      browser,
    }, testInfo) => {
      test.setTimeout(testInfo.timeout + environments.txTimeOut);

      const wallet = await ShelleyWallet.generate();
      const tempUserAuth = await createTempUserAuth(page, wallet);
      const governancePage = await createNewPageWithWallet(browser, {
        storageState: tempUserAuth,
        wallet,
        enableStakeSigning: true,
      });

      const proposalSubmissionPage = new ProposalSubmissionPage(governancePage);

      await proposalSubmissionPage.goto();

      await governancePage.getByTestId(`${type}-radio`).click();
      await proposalSubmissionPage.continueBtn.click();

      const proposal: IProposalForm =
        proposalSubmissionPage.generateValidProposalFormFields(
          type,
          wallet.rewardAddressBech32(environments.networkId)
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
