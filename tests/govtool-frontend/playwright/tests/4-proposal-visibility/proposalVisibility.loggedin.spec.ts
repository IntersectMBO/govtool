import { user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import { isMobile, openDrawerLoggedIn } from "@helpers/mobile";
import removeAllSpaces from "@helpers/removeAllSpaces";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { expect } from "@playwright/test";

const filterOptionNames = [
  "Protocol Parameter Change",
  "New Committee",
  "Hard Fork",
  "No Confidence",
  "Info Action",
  "Treasury Withdrawal",
  "Update to the Constitution",
];

enum SortOption {
  SoonToExpire = "SoonestToExpire",
  NewestFirst = "NewestCreated",
  HighestYesVotes = "MostYesVotes",
}

test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

test("4A.1: Should access Governance Actions page with connecting wallet @smoke @fast", async ({
  page,
}) => {
  await page.goto("/");
  if (isMobile(page)) {
    await openDrawerLoggedIn(page);
  }

  await page.getByTestId("governance-actions-link").click();
  await expect(page.getByText(/Governance Actions/i)).toHaveCount(2);
});

test("4B.1: Should restrict voting for users who are not registered as DReps (with wallet connected) @fast", async ({
  page,
}) => {
  const govActionsPage = new GovernanceActionsPage(page);
  await govActionsPage.goto();

  const govActionDetailsPage = await govActionsPage.viewFirstProposal();
  await expect(govActionDetailsPage.voteBtn).not.toBeVisible();
});

test("4C.1: Should filter Governance Action Type on governance actions page @slow", async ({
  page,
}) => {
  test.slow();

  const govActionsPage = new GovernanceActionsPage(page);
  await govActionsPage.goto();

  await govActionsPage.filterBtn.click();

  // Single filter
  for (const option of filterOptionNames) {
    await govActionsPage.filterProposalByNames([option]);
    await govActionsPage.validateFilters([option]);
    await govActionsPage.unFilterProposalByNames([option]);
  }

  // Multiple filters
  const multipleFilterOptionNames = [...filterOptionNames];
  while (multipleFilterOptionNames.length > 1) {
    await govActionsPage.filterProposalByNames(multipleFilterOptionNames);
    await govActionsPage.validateFilters(multipleFilterOptionNames);
    await govActionsPage.unFilterProposalByNames(multipleFilterOptionNames);
    multipleFilterOptionNames.pop();
  }
});

test("4C.2: Should sort Governance Action Type on governance actions page @slow", async ({
  page,
}) => {
  test.slow();

  const govActionsPage = new GovernanceActionsPage(page);
  await govActionsPage.goto();

  await govActionsPage.sortBtn.click();

  govActionsPage.sortProposal(SortOption.SoonToExpire);
  await govActionsPage.validateSort(
    SortOption.SoonToExpire,
    (p1, p2) => p1.expiryDate <= p2.expiryDate
  );

  govActionsPage.sortProposal(SortOption.NewestFirst);
  await govActionsPage.validateSort(
    SortOption.NewestFirst,
    (p1, p2) => p1.createdDate >= p2.createdDate
  );

  govActionsPage.sortProposal(SortOption.HighestYesVotes);
  await govActionsPage.validateSort(
    SortOption.HighestYesVotes,
    (p1, p2) => p1.yesVotes >= p2.yesVotes
  );
});

test("4D: Should filter and sort Governance Action Type on governance actions page @slow", async ({
  page,
}) => {
  test.slow();

  const govActionsPage = new GovernanceActionsPage(page);
  await govActionsPage.goto();

  await govActionsPage.sortBtn.click();
  await govActionsPage.sortProposal(SortOption.SoonToExpire);

  await govActionsPage.filterBtn.click();
  govActionsPage.filterProposalByNames([filterOptionNames[0]]);

  await govActionsPage.validateSort(
    SortOption.SoonToExpire,
    (p1, p2) => p1.expiryDate <= p2.expiryDate,
    [removeAllSpaces(filterOptionNames[0])]
  );
  await govActionsPage.validateFilters([filterOptionNames[0]]);
});
