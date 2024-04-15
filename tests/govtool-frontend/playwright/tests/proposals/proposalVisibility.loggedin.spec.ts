import { test } from "@fixtures/walletExtension";
import { user01Wallet } from "@constants/staticWallets";
import { expect } from "@playwright/test";
import { IProposal } from "@types";
import GovernanceActionsPage from "@pages/governanceActionsPage";

const mockProposals: IProposal[] = require("../../lib/_mock/proposals.json");

const filterOptions = [
  "Protocol Parameter Changes",
  "New Constitutional Committee or Quorum Size",
  "Hard Fork",
  "No Confidence",
  "Info Action",
  "Treasury Withdrawals",
  "Update to the Constitution",
];

test.describe("Logged in specs", () => {
  test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

  test("4A.1: Should access Governance Actions page with connecting wallet @smoke @fast", async ({
    page,
  }) => {
    await page.goto("/");

    await page.getByTestId("governance-actions-link").click();
    await expect(page.getByText(/Governance Actions/i)).toHaveCount(2);
  });

  test("4B.1: Should restrict voting for users who are not registered as DReps (with wallet connected) @critical @flaky @fast", async ({
    page,
  }) => {
    await page.route("**/proposal/list", async (route) => {
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify(mockProposals),
      });
    });

    const govActionsPage = new GovernanceActionsPage(page);
    await govActionsPage.goto();

    const govActionDetailsPage = await govActionsPage.viewProposal(
      mockProposals[0],
    );
    await expect(govActionDetailsPage.voteBtn).not.toBeVisible();
  });

  test("4C.1: Should filter Governance Action Type on governance actions page @slow @smoke", async ({
    page,
  }) => {
    const govActionsPage = new GovernanceActionsPage(page);
    await govActionsPage.goto();

    await govActionsPage.filterBtn.click();

    // Single filter
    filterOptions.forEach(async (option) => {
      await govActionsPage.filterProposalByName(option);

      // Validating filter
      const proposalCards = await page.getByTestId(`${option}-card`).all();
      proposalCards.forEach(async (proposalCard) => {
        await expect(proposalCard.getByText(option)).toBeVisible();
      });

      await govActionsPage.unFilterProposalByName(option);
    });

    // Multiple filters
  });

  test.skip("4C.2: Should sort Governance Action Type on governance actions page", () => {});

  test.skip("4C.3: Should filter and sort Governance Action Type on governance actions page", () => {});
});
