import { user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import extractExpiryDateFromText from "@helpers/extractExpiryDateFromText";
import { isMobile, openDrawer } from "@helpers/mobile";
import removeAllSpaces from "@helpers/removeAllSpaces";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { expect } from "@playwright/test";

const infoTypeProposal = require("../../lib/_mock/infoTypeProposal.json");

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

test.beforeEach(async () => {
  await setAllureEpic("4. Proposal visibility");
});

test("4A_1. Should access Governance Actions page with connecting wallet", async ({
  page,
}) => {
  await page.goto("/");
  if (isMobile(page)) {
    await openDrawer(page);
  }

  await page.getByTestId("governance-actions-link").click();
  await expect(page.getByText(/Governance Actions/i)).toHaveCount(2);
});

test("4B_1. Should restrict voting for users who are not registered as DReps (with wallet connected)", async ({
  page,
}) => {
  const govActionsPage = new GovernanceActionsPage(page);
  await govActionsPage.goto();

  const govActionDetailsPage = await govActionsPage.viewFirstProposal();
  await expect(govActionDetailsPage.voteBtn).not.toBeVisible();
});

test("4C_1. Should filter Governance Action Type on governance actions page", async ({
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

test("4C_2. Should sort Governance Action Type on governance actions page", async ({
  page,
}) => {
  test.slow();

  const govActionsPage = new GovernanceActionsPage(page);
  await govActionsPage.goto();

  await govActionsPage.sortBtn.click();

  await govActionsPage.sortAndValidate(
    SortOption.SoonToExpire,
    (p1, p2) => p1.expiryDate <= p2.expiryDate
  );

  await govActionsPage.sortAndValidate(
    SortOption.NewestFirst,
    (p1, p2) => p1.createdDate >= p2.createdDate
  );

  await govActionsPage.sortAndValidate(
    SortOption.HighestYesVotes,
    (p1, p2) => p1.yesVotes >= p2.yesVotes
  );
});

test("4C_3. Should filter and sort Governance Action Type on governance actions page", async ({
  page,
}) => {
  test.slow();

  const govActionsPage = new GovernanceActionsPage(page);
  await govActionsPage.goto();

  await govActionsPage.filterBtn.click();

  const choice = Math.floor(Math.random() * filterOptionNames.length);
  await govActionsPage.filterProposalByNames([filterOptionNames[choice]]);

  await govActionsPage.sortBtn.click();
  await govActionsPage.sortAndValidate(
    SortOption.SoonToExpire,
    (p1, p2) => p1.expiryDate <= p2.expiryDate,
    [removeAllSpaces(filterOptionNames[0])]
  );
  await govActionsPage.validateFilters([filterOptionNames[0]]);
});

test("4L. Should search governance actions", async ({ page }) => {
  const governanceActionTitle = "TreasuryTitle";
  const governanceActionPage = new GovernanceActionsPage(page);

  await governanceActionPage.goto();

  await governanceActionPage.searchInput.fill(governanceActionTitle);

  const proposalCards = await governanceActionPage.getAllProposals();

  for (const proposalCard of proposalCards) {
    expect(
      (await proposalCard.textContent()).includes(`${governanceActionTitle}`)
    ).toBeTruthy();
  }
});

test("4M. Should show view-all categorized governance actions", async ({
  page,
}) => {
  await page.route("**/proposal/list?**", async (route) =>
    route.fulfill({
      body: JSON.stringify(infoTypeProposal),
    })
  );

  const governanceActionPage = new GovernanceActionsPage(page);
  await governanceActionPage.goto();

  await page.getByRole("button", { name: "Show All" }).click();

  const proposalCards = await governanceActionPage.getAllProposals();

  for (const proposalCard of proposalCards) {
    await expect(proposalCard.getByTestId("InfoAction-type")).toBeVisible();
  }
});

test("4H. Should verify none of the displayed governance actions have expired", async ({
  page,
}) => {
  const govActionsPage = new GovernanceActionsPage(page);
  await govActionsPage.goto();

  const proposalCards = await govActionsPage.getAllProposals();

  for (const proposalCard of proposalCards) {
    const expiryDateEl = proposalCard.getByTestId("expiry-date");
    const expiryDateTxt = await expiryDateEl.innerText();
    const expiryDate = extractExpiryDateFromText(expiryDateTxt);
    const today = new Date();
    expect(today <= expiryDate).toBeTruthy();
  }
});
