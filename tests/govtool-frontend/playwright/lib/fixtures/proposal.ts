import { proposal01AuthFile } from "@constants/auth";
import { proposal01Wallet } from "@constants/staticWallets";
import { test as base } from "@fixtures/walletExtension";
import { createNewPageWithWallet } from "@helpers/page";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import ProposalSubmissionPage from "@pages/proposalSubmissionPage";

type TestOptions = {
  proposalId: number;
  pollEnabled: boolean;
};

export const test = base.extend<TestOptions>({
  pollEnabled: [false, { option: true }],

  proposalId: async ({ browser, pollEnabled }, use) => {
    // setup
    const proposalPage = await createNewPageWithWallet(browser, {
      storageState: proposal01AuthFile,
      wallet: proposal01Wallet,
    });

    const proposalCreationPage = new ProposalSubmissionPage(proposalPage);
    await proposalCreationPage.goto();

    const proposalId =
      await proposalCreationPage.createProposal(proposal01Wallet);

    const proposalDetailsPage = new ProposalDiscussionDetailsPage(proposalPage);

    if (pollEnabled) {
      await proposalDetailsPage.addPollBtn.click();
    }

    await use(proposalId);

    // cleanup
    await proposalDetailsPage.deleteProposal();
  },
});
