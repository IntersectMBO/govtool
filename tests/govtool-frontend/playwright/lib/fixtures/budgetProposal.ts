import { budgetProposal01AuthFile } from "@constants/auth";
import { budgetProposal01Wallet } from "@constants/staticWallets";
import { test as base } from "@fixtures/walletExtension";
import { createNewPageWithWallet } from "@helpers/page";
import BudgetDiscussionDetailsPage from "@pages/budgetDiscussionDetailsPage";
import BudgetDiscussionSubmissionPage from "@pages/budgetDiscussionSubmissionPage";

type TestOptions = {
  proposalId: number;
};

export const test = base.extend<TestOptions>({
  proposalId: async ({ browser }, use) => {
    // setup
    const budgetProposalPage = await createNewPageWithWallet(browser, {
      storageState: budgetProposal01AuthFile,
      wallet: budgetProposal01Wallet,
    });

    const budgetProposalCreationPage = new BudgetDiscussionSubmissionPage(
      budgetProposalPage
    );
    await budgetProposalCreationPage.goto();

    const { proposalId } =
      await budgetProposalCreationPage.createBudgetProposal();

    const budgetProposalDetailsPage = new BudgetDiscussionDetailsPage(
      budgetProposalPage
    );

    await use(proposalId);

    // cleanup
    await budgetProposalDetailsPage.deleteProposal();
  },
});
