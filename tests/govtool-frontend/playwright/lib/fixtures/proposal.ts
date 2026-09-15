import { proposal01AuthFile } from "@constants/auth";
import environments from "@constants/environments";
import { proposal01Wallet } from "@constants/staticWallets";
import { test as base } from "@fixtures/walletExtension";
import { createNewPageWithWallet } from "@helpers/page";
import { rewardAddressBech32 } from "@helpers/shellyWallet";
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

    const receiverAddress = rewardAddressBech32(
      environments.networkId,
      proposal01Wallet.stake.pkh
    );

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
