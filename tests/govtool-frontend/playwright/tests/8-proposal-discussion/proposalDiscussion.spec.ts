import { faker } from "@faker-js/faker";
import { test } from "@fixtures/proposal";
import { setAllureEpic } from "@helpers/allure";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import { expect } from "@playwright/test";

const mockProposal = require("../../lib/_mock/proposal.json");
const mockPoll = require("../../lib/_mock/poll.json");

test.beforeEach(() => {
  setAllureEpic("Proposal Discussion Forum");
});

test("8A. Should access proposed governance actions in disconnected state", async ({
  page,
}) => {
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  await expect(page.getByText(/Proposed Governance Actions/i)).toHaveCount(2);
});

test("8B. Should filter and sort the list of proposed governance actions.", async ({
  page,
}) => {
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  await proposalDiscussionPage.filterBtn.click();
  await proposalDiscussionPage.infoRadio.click();

  await expect(page.getByText("Treasury")).toHaveCount(1);

  await proposalDiscussionPage.treasuryRadio.click();
});

test("8C. Should search the list of proposed governance actions.", async ({
  page,
}) => {
  const proposalName = "Labadie, Stehr and Rosenbaum";
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  await proposalDiscussionPage.searchInput.fill(proposalName);

  const proposalCards = await proposalDiscussionPage.getAllProposals();

  for (const proposalCard of proposalCards) {
    await expect(proposalCard.getByText(proposalName)).toBeVisible();
  }
});

test("8D.Should show the view-all categorized proposed governance actions.", async ({
  page,
}) => {
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  await proposalDiscussionPage.showAllBtn.click();

  await expect(proposalDiscussionPage.showLessBtn).toBeVisible();
});

test("8H. Should disable proposal interaction on a disconnected state.", async ({
  page,
}) => {
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  const proposalDiscussionDetailsPage =
    await proposalDiscussionPage.viewFirstProposal();

  await proposalDiscussionDetailsPage.commentInput.fill(
    faker.lorem.paragraph()
  );

  await expect(proposalDiscussionDetailsPage.likeBtn).toBeDisabled();
  await expect(proposalDiscussionDetailsPage.dislikeBtn).toBeDisabled();
  await expect(proposalDiscussionDetailsPage.commentBtn).toBeDisabled();
});

test("8R. Should restrict proposal creation on disconnected state", async ({
  page,
}) => {
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  await expect(proposalDiscussionPage.proposalCreateBtn).not.toBeVisible();
});

test.describe("Mocked proposal", () => {
  let proposalDiscussionDetailsPage: ProposalDiscussionDetailsPage;

  test.beforeEach(async ({ page }) => {
    await page.route("**/api/proposals/**", async (route) =>
      route.fulfill({
        body: JSON.stringify(mockProposal),
      })
    );

    await page.route("**/api/polls/**", async (route) =>
      route.fulfill({
        body: JSON.stringify(mockPoll),
      })
    );

    proposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(page);
    await proposalDiscussionDetailsPage.goto(10);
  });

  test("8E. Should share proposed governance action", async ({
    page,
    context,
  }) => {
    await context.grantPermissions(["clipboard-read", "clipboard-write"]);

    await page.getByTestId("share-button").click();
    await expect(page.getByText("Copied to clipboard")).toBeVisible();
    const copiedTextDRepDirectory = await page.evaluate(() =>
      navigator.clipboard.readText()
    );
    expect(copiedTextDRepDirectory).toEqual(mockProposal.data.id);
  });

  test("8I. Should disable poll voting functionality.", async () => {
    await expect(proposalDiscussionDetailsPage.pollVoteCard).not.toBeVisible();
    await expect(proposalDiscussionDetailsPage.pollYesBtn).not.toBeVisible();

    await expect(proposalDiscussionDetailsPage.pollNoBtn).not.toBeVisible();
  });
});
