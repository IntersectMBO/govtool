import { proposal01Wallet } from "@constants/staticWallets";
import { test as base } from "@fixtures/walletExtension";
import { createNewPageWithWallet } from "@helpers/page";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";

type TestOptions = {
  proposalId: number;
};

export const test = base.extend<TestOptions>({
  proposalId: async ({ page, browser }, use) => {
    // setup
    const proposalPage = await createNewPageWithWallet(browser, {
      storageState: ".auth/proposal01.json",
      wallet: proposal01Wallet,
    });

    const proposalDiscussionPage = new ProposalDiscussionPage(proposalPage);
    await proposalDiscussionPage.goto();
    const proposalId = await proposalDiscussionPage.createProposal();

    await use(proposalId);

    // cleanup
    const proposalDetailsPage = new ProposalDiscussionDetailsPage(proposalPage);
    await proposalDetailsPage.goto(proposalId);
    await proposalDetailsPage.deleteProposal();
  },
});
