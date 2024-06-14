import { proposal01Wallet } from "@constants/staticWallets";
import { test as base } from "@fixtures/walletExtension";
import { createNewPageWithWallet } from "@helpers/page";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";

type TestOptions = {
  proposalDiscussionDetailsPage: ProposalDiscussionDetailsPage;
};

export const test = base.extend<TestOptions>({
  proposalDiscussionDetailsPage: async ({ page, browser }, use) => {
    // setup
    const proposalPage = await createNewPageWithWallet(browser, {
      storageState: ".auth/proposal01.json",
      wallet: proposal01Wallet,
    });

    const proposalDiscussionPage = new ProposalDiscussionPage(proposalPage);
    await proposalDiscussionPage.goto();
    const proposalId = await proposalDiscussionPage.createProposal();

    const userProposalDetailsPage = new ProposalDiscussionDetailsPage(page);
    await userProposalDetailsPage.goto(proposalId);
    await page
      .locator("div")
      .filter({ hasText: /^Hey, setup your username$/ })
      .getByRole("button")
      .click();
    await use(userProposalDetailsPage);

    // cleanup
    const ownerProposalDetailsPage = new ProposalDiscussionDetailsPage(
      proposalPage
    );
    await ownerProposalDetailsPage.goto(proposalId);
    await ownerProposalDetailsPage.deleteProposal();
  },
});
