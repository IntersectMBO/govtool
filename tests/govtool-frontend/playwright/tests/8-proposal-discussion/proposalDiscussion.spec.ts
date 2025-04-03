import environments from "@constants/environments";
import {
  BOOTSTRAP_PROPOSAL_TYPE_FILTERS,
  PROPOSAL_STATUS_FILTER,
} from "@constants/index";
import { faker } from "@faker-js/faker";
import { test } from "@fixtures/proposal";
import { setAllureEpic } from "@helpers/allure";
import { isBootStrapingPhase, skipIfNotHardFork } from "@helpers/cardano";
import { injectLogger } from "@helpers/page";
import { extractProposalIdFromUrl } from "@helpers/string";
import { functionWaitedAssert } from "@helpers/waitedLoop";
import ProposalDiscussionDetailsPage from "@pages/proposalDiscussionDetailsPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import { expect } from "@playwright/test";
import { ProposalType } from "@types";

const mockProposal = require("../../lib/_mock/proposal.json");
const mockPoll = require("../../lib/_mock/proposalPoll.json");
const mockComments = require("../../lib/_mock/proposalComments.json");

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

    await proposalDiscussionPage.filterBtn.click();

    // unselect active proposal
    await proposalDiscussionPage.activeProposalWrapper.click();
    const isBootStraping = await isBootStrapingPhase();

    // proposal type filter
    await proposalDiscussionPage.applyAndValidateFilters(
      isBootStraping
        ? BOOTSTRAP_PROPOSAL_TYPE_FILTERS
        : Object.values(ProposalType),
      proposalDiscussionPage._validateTypeFiltersInProposalCard
    );

    // proposal status filter
    await proposalDiscussionPage.applyAndValidateFilters(
      PROPOSAL_STATUS_FILTER,
      proposalDiscussionPage._validateStatusFiltersInProposalCard
    );
  });

  test("8B_2. Should sort the list of proposed governance actions.", async () => {
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
        expect(proposalTitle).toContain(proposalName);
      }
    },
    {
      message: `A proposal card does not contain the search term ${proposalName}`,
    }
  );
});

test("8D. Should show the view-all categorized proposed governance actions.", async ({
  browser,
}) => {
  await Promise.all(
    Object.values(ProposalType).map(async (proposalType: string) => {
      const context = await browser.newContext();
      const page = await context.newPage();
      injectLogger(page);

      const proposalDiscussionPage = new ProposalDiscussionPage(page);
      await proposalDiscussionPage.goto();
      const isShowAllButtonVisible = await page
        .waitForSelector(
          `[data-testid="${proposalType.toLowerCase().replace(/ /g, "-")}-show-all-button"]`,
          { timeout: 60_000 }
        )
        .then(() => true)
        .catch(() => false);

      if (isShowAllButtonVisible) {
        await page
          .getByTestId(
            proposalType.toLowerCase().replace(/ /g, "-") + "-show-all-button"
          )
          .click();

        const proposalCards = await proposalDiscussionPage.getAllProposals();

        for (const proposalCard of proposalCards) {
          await expect(
            proposalCard.getByTestId("governance-action-type")
          ).toHaveText(proposalType, { timeout: 60_000 });
        }
      } else {
        expect(true, `No ${proposalType} found`).toBeTruthy();
      }
    })
  );
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
    await expect(proposalDiscussionDetailsPage.commentCount).toHaveText(
      mockProposal.data.attributes.prop_comments_number.toString()
    );
  });
});
