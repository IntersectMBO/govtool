import { test as base } from "@fixtures/walletExtension";
import { createNewPageWithWallet } from "@helpers/page";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import ProposalSubmissionPage from "@pages/proposalSubmissionPage";
import { testWallet } from "lib/wallet/testWallets";

type TestOptions = {
  proposalId: number;
  pollEnabled: boolean;
};

export const test = base.extend<TestOptions>({
  pollEnabled: [false, { option: true }],

  proposalId: async ({ browser, pollEnabled }, use) => {
    // setup
    const proposalWallet = await testWallet("proposal01");
    const proposalPage = await createNewPageWithWallet(browser, {
      wallet: proposalWallet,
    });

    const proposalCreationPage = new ProposalSubmissionPage(proposalPage);
    await proposalCreationPage.goto();

    const receiverAddress = proposalWallet.stakeAddress;

    const proposalId =
      await proposalCreationPage.createProposal(receiverAddress);

    const proposalDetailsPage = new ProposalDiscussionDetailsPage(proposalPage);

    if (pollEnabled) {
      await proposalDetailsPage.addPollBtn.click();
    }

    await use(proposalId);

    // cleanup
    await proposalDetailsPage.deleteProposal();
  },
});
