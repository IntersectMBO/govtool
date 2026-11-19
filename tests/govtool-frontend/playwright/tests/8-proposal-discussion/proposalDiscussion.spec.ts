import environments from "@constants/environments";
import { BOOTSTRAP_PROPOSAL_TYPE_FILTERS } from "@constants/index";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/proposal";
import { setAllureEpic } from "@helpers/allure";
import { isBootStrapingPhase } from "@helpers/cardano";
import { getProposalType } from "@helpers/index";
import { extractProposalIdFromUrl } from "@helpers/string";
import { functionWaitedAssert } from "@helpers/waitedLoop";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import { expect } from "@playwright/test";
import {
  ProposalDiscussionFilterTypes,
  ProposalType,
  ProposedGovAction,
} from "@types";

const mockProposal = require("../../lib/_mock/proposal.json");
const mockPoll = require("../../lib/_mock/proposalPoll.json");
const mockComments = require("../../lib/_mock/proposalComments.json");

test.beforeEach(async () => {
  await setAllureEpic("8. Proposal Discussion Forum");
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

    const isBootStraping = await isBootStrapingPhase();

    // proposal type filter, in the action type sidebar
    await proposalDiscussionPage.applyAndValidateFilters(
      isBootStraping ? BOOTSTRAP_PROPOSAL_TYPE_FILTERS : getProposalType(),
      proposalDiscussionPage._validateTypeFiltersInProposalCard
    );
  });

  test("8B_2. Should sort the list of proposed governance actions.", async () => {
    test.slow();
    // The Date Range menu: Latest and Oldest.
    const sortOptions = {
      Oldest: (p1: ProposedGovAction, p2: ProposedGovAction) =>
        p1.attributes.content.attributes.createdAt <=
        p2.attributes.content.attributes.createdAt,
      Latest: (p1: ProposedGovAction, p2: ProposedGovAction) =>
        p1.attributes.content.attributes.createdAt >=
        p2.attributes.content.attributes.createdAt,
    };

    for (const [sortOption, sortFunction] of Object.entries(sortOptions)) {
      await proposalDiscussionPage.sortAndValidate(
        sortOption as ProposalDiscussionFilterTypes,
        sortFunction
      );
    }
  });
});

test("8C. Should search the list of proposed governance actions.", async ({
  page,
}) => {
  let proposalName = "Labadie, Stehr and Rosenbaum";
  let proposalNameSet = false;

  await page.route("**/api/proposals?**", async (route) => {
    const response = await route.fetch();
    const json = await response.json();
    if (!proposalNameSet && "data" in json && json["data"].length > 0) {
      const randomIndex = Math.floor(Math.random() * json["data"].length);
      proposalName =
        json["data"][randomIndex]["attributes"]["content"]["attributes"][
          "prop_name"
        ];
      proposalNameSet = true;
    }
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify(json),
    });
  });

  const responsePromise = page.waitForResponse("**/api/proposals?**");
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  await responsePromise;

  await proposalDiscussionPage.searchInput.fill(proposalName);

  await page.waitForTimeout(2000);

  await functionWaitedAssert(
    async () => {
      const proposalCards = await proposalDiscussionPage.getAllProposals();
      for (const proposalCard of proposalCards) {
        await expect(proposalCard).toBeVisible();
        const proposalTitle = await proposalCard
          .locator('[data-testid^="proposal-"][data-testid$="-title"]')
          .innerText();
        expect(proposalTitle.toLowerCase().trim()).toContain(
          proposalName.toLowerCase().trim()
        );
      }
    },
    {
      message: `A proposal card does not contain the search term ${proposalName}`,
    }
  );
});

test("8D. Should list one type of proposed governance action through the type filter.", async ({
  page,
}) => {
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  for (const proposalType of getProposalType()) {
    const filtered = page.waitForResponse(
      (response) =>
        response.url().includes("/api/proposals?") &&
        response.url().includes("pageSize]=10") &&
        decodeURIComponent(response.url()).includes("[gov_action_type_id][$in]")
    );
    await proposalDiscussionPage.filterProposalByNames([proposalType]);
    await filtered;
    // The previous rows stay, dimmed, until the new page renders.
    await expect(page.locator('[aria-busy="true"]')).toHaveCount(0);

    const isEmpty = await page
      .getByTestId("proposals-empty-state")
      .isVisible();
    if (!isEmpty) {
      const proposalCards = await proposalDiscussionPage.getAllProposals();
      for (const proposalCard of proposalCards) {
        await expect(
          proposalCard.getByTestId("governance-action-type")
        ).toHaveText(proposalType, { timeout: 60_000 });
      }
    }

    await proposalDiscussionPage.unFilterProposalByNames([proposalType]);
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

test("8S. Should ask a disconnected visitor to connect before proposing", async ({
  page,
}) => {
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  // The header's notice says what proposing needs; the button connects.
  await expect(page.getByTestId("connect-wallet-link")).toBeVisible();
  await proposalDiscussionPage.proposalCreateBtn.click();
  await expect(page.getByTestId("connect-your-wallet-modal")).toBeVisible();
});

test("8E. Should share proposed governance action", async ({
  page,
  context,
}) => {
  await context.grantPermissions(["clipboard-read", "clipboard-write"]);
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  await proposalDiscussionPage.viewFirstProposal();

  const currentPageUrl = page.url();
  const proposalId = extractProposalIdFromUrl(currentPageUrl);

  await page.getByTestId("share-button").click();
  await page.getByTestId("copy-link").click();
  await expect(page.getByTestId("copy-link-text")).toBeVisible();

  const copiedTextDRepDirectory = await page.evaluate(() =>
    navigator.clipboard.readText()
  );
  const expectedCopyUrl = `${environments.frontendUrl}/proposal_discussion/${proposalId}`;

  expect(copiedTextDRepDirectory).toEqual(expectedCopyUrl);
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

  test("8I. Should disable poll voting functionality.", async () => {
    await expect(proposalDiscussionDetailsPage.pollVoteCard).not.toBeVisible();
    await expect(proposalDiscussionDetailsPage.pollYesBtn).not.toBeVisible();

    await expect(proposalDiscussionDetailsPage.pollNoBtn).not.toBeVisible();
  });

  test("8F. Should display all comments with count indication.", async () => {
    let commentCount = mockProposal.data.attributes.prop_comments_number;
    if (commentCount > 99) {
      commentCount = "99+";
    }
    await expect(proposalDiscussionDetailsPage.commentCount).toHaveText(
      commentCount.toString(),
      { timeout: 60_000 }
    );
  });
});
