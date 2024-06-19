import { proposal01Wallet } from "@constants/staticWallets";
import { test as base } from "@fixtures/walletExtension";
import { createNewPageWithWallet } from "@helpers/page";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";

type TestOptions = {
  proposalId: number;
  pollEnabled: boolean;
};

export const test = base.extend<TestOptions>({
  pollEnabled: [false, { option: true }],

  proposalId: async ({ page, browser, pollEnabled }, use) => {
    // setup
    const proposalPage = await createNewPageWithWallet(browser, {
      storageState: ".auth/proposal01.json",
      wallet: proposal01Wallet,
    });

    const proposalDiscussionPage = new ProposalDiscussionPage(proposalPage);
    await proposalDiscussionPage.goto();
    const proposalId = await proposalDiscussionPage.createProposal();
    const proposalDetailsPage = new ProposalDiscussionDetailsPage(proposalPage);

    if (pollEnabled) {
      await proposalDetailsPage.addPollBtn.click();
    }

    await use(proposalId);

    // cleanup
    await proposalDetailsPage.goto(proposalId);
    await proposalDetailsPage.deleteProposal();
  },
});
