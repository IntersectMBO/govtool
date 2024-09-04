import environments from "@constants/environments";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/proposal";
import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import { expect } from "@playwright/test";

const mockProposal = require("../../lib/_mock/proposal.json");
const mockPoll = require("../../lib/_mock/proposalPoll.json");
const mockComments = require("../../lib/_mock/proposalComments.json");
const mockInfoProposedGA = require("../../lib/_mock/infoProposedGAs.json");

const PROPOSAL_TYPE_FILTERS = ["Info", "Treasury"];

const PROPOSAL_STATUS_FILTER = ["Submitted for vote", "Active proposal"];

test.beforeEach(async () => {
  await setAllureEpic("8. Proposal Discussion Forum");
  await skipIfNotHardFork();
});

test("8A. Should access proposed governance actions in disconnected state", async ({
  page,
}) => {
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  await expect(page.getByText(/Proposed Governance Actions/i)).toHaveCount(1);
});

test.describe("Filter and sort proposals", () => {
  let proposalDiscussionPage: ProposalDiscussionPage;

  test.beforeEach(async ({ page }) => {
    proposalDiscussionPage = new ProposalDiscussionPage(page);
    await proposalDiscussionPage.goto();
  });

  test("8B_1. Should filter the list of proposed governance actions.", async () => {
    test.slow();

    // unselect active proposal
    await proposalDiscussionPage.activeProposalWrapper.click();

    // proposal type filter
    await proposalDiscussionPage.applyAndValidateFilters(
      PROPOSAL_TYPE_FILTERS,
      proposalDiscussionPage._validateTypeFiltersInProposalCard
    );

    // proposal status filter
    await proposalDiscussionPage.applyAndValidateFilters(
      PROPOSAL_STATUS_FILTER,
      proposalDiscussionPage._validateStatusFiltersInProposalCard
    );
  });

  test("8B_2. Should sort the list of proposed governance actions.", async () => {
    await proposalDiscussionPage.sortBtn.click();

    await proposalDiscussionPage.sortAndValidate(
      "asc",
      (p1, p2) => p1.attributes.createdAt <= p2.attributes.createdAt
    );

    await proposalDiscussionPage.sortAndValidate(
      "desc",
      (p1, p2) => p1.attributes.createdAt >= p2.attributes.createdAt
    );
  });
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
    await expect(
      proposalCard.locator('[data-testid^="proposal-"][data-testid$="-title"]')
    ).toHaveText(proposalName);
  }
});

test("8D.Should show the view-all categorized proposed governance actions.", async ({
  page,
}) => {
  await page.route("**/api/proposals?**", async (route) => {
    return route.fulfill({
      body: JSON.stringify(mockInfoProposedGA),
    });
  });

  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  await proposalDiscussionPage.showAllBtn.click();

  const proposalCards = await proposalDiscussionPage.getAllProposals();

  for (const proposalCard of proposalCards) {
    await expect(
      proposalCard.getByTestId("governance-action-type")
    ).toBeVisible();
  }
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

test("8S. Should restrict proposal creation on disconnected state", async ({
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

    await page.route("**/api/polls**", async (route) =>
      route.fulfill({
        body: JSON.stringify(mockPoll),
      })
    );

    await page.route("**/api/comments**", async (route) =>
      route.fulfill({
        body: JSON.stringify(mockComments),
      })
    );

    proposalDiscussionDetailsPage = new ProposalDiscussionDetailsPage(page);
    await proposalDiscussionDetailsPage.goto(mockProposal.data.id);
  });

  test("8E. Should share proposed governance action", async ({
    page,
    context,
  }) => {
    await context.grantPermissions(["clipboard-read", "clipboard-write"]);

    await page.getByTestId("share-button").click();
    await page.getByTestId("copy-link").click();
    await expect(page.getByTestId("copy-link-text")).toBeVisible();

    const copiedTextDRepDirectory = await page.evaluate(() =>
      navigator.clipboard.readText()
    );
    const expectedCopyUrl = `${environments.frontendUrl}/proposal_discussion/${mockProposal.data.id}`;

    expect(copiedTextDRepDirectory).toEqual(expectedCopyUrl);
  });

  test("8I. Should disable poll voting functionality.", async () => {
    await expect(proposalDiscussionDetailsPage.pollVoteCard).not.toBeVisible();
    await expect(proposalDiscussionDetailsPage.pollYesBtn).not.toBeVisible();

    await expect(proposalDiscussionDetailsPage.pollNoBtn).not.toBeVisible();
  });

  test("8F. Should display all comments with count indication.", async () => {
    await expect(proposalDiscussionDetailsPage.commentCount).toHaveText(
      mockProposal.data.attributes.prop_comments_number.toString()
    );
  });
});
