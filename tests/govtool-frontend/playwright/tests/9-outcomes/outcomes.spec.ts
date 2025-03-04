import { test } from "@fixtures/walletExtension";
import { correctVoteAdaFormat } from "@helpers/adaFormat";
import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import extractExpiryDateFromText from "@helpers/extractExpiryDateFromText";
import {
  areCCVoteTotalsDisplayed,
  areDRepVoteTotalsDisplayed,
  areSPOVoteTotalsDisplayed,
} from "@helpers/featureFlag";
import { isMobile } from "@helpers/mobile";
import { injectLogger } from "@helpers/page";
import { functionWaitedAssert } from "@helpers/waitedLoop";
import OutComesPage from "@pages/outcomesPage";
import { expect, Page } from "@playwright/test";
import { outcomeMetadata, outcomeProposal, outcomeType } from "@types";

test.beforeEach(async () => {
  await setAllureEpic("9. Outcomes");
  await skipIfNotHardFork();
});

const status = ["Expired", "Ratified", "Enacted", "Live"];

enum SortOption {
  SoonToExpire = "Soon to expire",
  NewestFirst = "Newest first",
  OldestFirst = "Oldest first",
  HighestAmountYesVote = "Highest amount of yes votes",
}
test("9A. Should access Outcomes page in disconnect state", async ({
  page,
}) => {
  await page.goto("/");

  if (isMobile(page)) {
    await page.getByTestId("open-drawer-button").click();
  }
  await page.getByTestId("governance-actions-outcomes-link").click();

  await expect(page.getByText(/outcomes/i)).toHaveCount(2);
});

test.describe("outcome details dependent test", () => {
  let governanceActionId: string | undefined;
  let governanceActionTitle: string | undefined;
  let currentPage: Page;
  test.beforeEach(async ({ page }) => {
    // intercept outcomes data for id
    await page.route(
      "**/governance-actions?search=&filters=&sort=**",
      async (route) => {
        const response = await route.fetch();
        const data: outcomeProposal[] = await response.json();
        if (!governanceActionId) {
          if (data.length > 0) {
            const randomIndexForId = Math.floor(Math.random() * data.length);
            governanceActionId =
              data[randomIndexForId].tx_hash +
              "#" +
              data[randomIndexForId].index;
          }
        }
        await route.fulfill({
          status: 200,
          contentType: "application/json",
          body: JSON.stringify(data),
        });
      }
    );

    // intercept metadata for title
    await page.route("**/governance-actions/metadata?**", async (route) => {
      try {
        const response = await route.fetch();
        if (response.status() !== 200) {
          await route.continue();
          return;
        }
        const data: outcomeMetadata = await response.json();
        if (!governanceActionTitle && data.body.title != null) {
          governanceActionTitle = data.body.title;
        }
        await route.fulfill({
          status: 200,
          contentType: "application/json",
          body: JSON.stringify(data),
        });
      } catch (error) {
        return;
      }
    });

    const responsePromise = page.waitForResponse(
      "**/governance-actions?search=&filters=&sort=**"
    );
    const metadataResponsePromise = page.waitForResponse(
      "**/governance-actions/metadata?**"
    );

    const outcomesPage = new OutComesPage(page);
    await outcomesPage.goto();

    await responsePromise;
    await metadataResponsePromise;
    currentPage = page;
  });

  test("9B. Should search outcomes proposal by title and id", async ({}) => {
    const outcomesPage = new OutComesPage(currentPage);
    // search by id
    await outcomesPage.searchInput.fill(governanceActionId);
    await expect(
      currentPage.getByRole("progressbar").getByRole("img")
    ).toBeVisible();

    await functionWaitedAssert(
      async () => {
        const idSearchOutcomeCards = await outcomesPage.getAllOutcomes();
        expect(idSearchOutcomeCards.length, {
          message:
            idSearchOutcomeCards.length == 0 && "No governance actions found",
        }).toBeGreaterThan(0);
        for (const outcomeCard of idSearchOutcomeCards) {
          const id = await outcomeCard
            .locator('[data-testid$="-CIP-105-id"]')
            .textContent();
          expect(id.replace(/^.*ID/, "")).toContain(governanceActionId);
        }
      },
      { name: "search by id" }
    );

    // search by title
    await outcomesPage.searchInput.fill(governanceActionTitle);
    await expect(
      currentPage.getByRole("progressbar").getByRole("img")
    ).toBeVisible();

    await functionWaitedAssert(
      async () => {
        const titleSearchOutcomeCards = await outcomesPage.getAllOutcomes();
        expect(titleSearchOutcomeCards.length, {
          message:
            titleSearchOutcomeCards.length == 0 &&
            "No governance actions found",
        }).toBeGreaterThan(0);
        for (const outcomeCard of titleSearchOutcomeCards) {
          const title = await outcomeCard
            .locator('[data-testid$="-card-title"]')
            .textContent();
          expect(title).toContain(governanceActionTitle);
        }
      },
      { name: "search by title" }
    );
  });

  test("9D. Should copy governanceActionId", async ({ page, context }) => {
    await context.grantPermissions(["clipboard-read", "clipboard-write"]);
    const outcomesPage = new OutComesPage(currentPage);

    await outcomesPage.searchInput.fill(governanceActionId);
    await expect(
      currentPage.getByRole("progressbar").getByRole("img")
    ).toBeVisible();
    await page
      .getByTestId(`${governanceActionId}-CIP-105-id`)
      .getByTestId("copy-button")
      .click();
    await expect(page.getByText("Copied to clipboard")).toBeVisible({
      timeout: 10_000,
    });
    const copiedTextDRepDirectory = await page.evaluate(() =>
      navigator.clipboard.readText()
    );
    expect(copiedTextDRepDirectory).toEqual(governanceActionId);
  });
});

test("9C_1. Should filter Governance Action Type on governance actions page", async ({
  page,
}) => {
  test.slow();

  const outcomePage = new OutComesPage(page);
  await outcomePage.goto();

  await outcomePage.filterBtn.click();
  const filterOptionNames = Object.values(outcomeType);

  // proposal type filter
  await outcomePage.applyAndValidateFilters(
    filterOptionNames,
    outcomePage._validateFiltersInOutcomeCard
  );

  // proposal status filter
  await outcomePage.applyAndValidateFilters(
    status,
    outcomePage._validateStatusFiltersInOutcomeCard
  );
});

test("9C_2. Should sort Governance Action Type on outcomes page", async ({
  page,
}) => {
  test.slow();

  const outcomePage = new OutComesPage(page);
  await outcomePage.goto();

  await outcomePage.sortBtn.click();

  await outcomePage.sortAndValidate(
    SortOption.OldestFirst,
    (p1, p2) => p1.expiry_date <= p2.expiry_date
  );

  await outcomePage.sortAndValidate(
    SortOption.NewestFirst,
    (p1, p2) => p1.expiry_date >= p2.time
  );

  await outcomePage.sortAndValidate(
    SortOption.HighestAmountYesVote,
    (p1, p2) => parseInt(p1.yes_votes) >= parseInt(p2.yes_votes)
  );
});

test("9C_3. Should filter and sort Governance Action Type on outcomes page", async ({
  page,
}) => {
  const outcomePage = new OutComesPage(page);
  const filterOptionKeys = Object.keys(outcomeType);
  const filterOptionNames = Object.values(outcomeType);

  const choice = Math.floor(Math.random() * filterOptionKeys.length);
  await outcomePage.goto(filterOptionKeys[choice]);
  await outcomePage.sortBtn.click();

  await outcomePage.sortAndValidate(
    SortOption.OldestFirst,
    (p1, p2) => p1.expiry_date <= p2.expiry_date
  );

  await outcomePage.validateFilters(
    [filterOptionNames[choice]],
    outcomePage._validateFiltersInOutcomeCard
  );
});

test("9E. Should verify all of the displayed governance actions have expired", async ({
  page,
}) => {
  const outcomePage = new OutComesPage(page);
  await outcomePage.goto();

  const proposalCards = await outcomePage.getAllOutcomes();

  for (const proposalCard of proposalCards) {
    const expiryDateEl = proposalCard.locator('[data-testid$="-Expired-date"]');
    const expiryDateTxt = await expiryDateEl.innerText();
    const expiryDate = extractExpiryDateFromText(expiryDateTxt, true);
    const today = new Date();
    expect(today >= expiryDate).toBeTruthy();
  }
});

test("9F. Should load more Outcomes on show more", async ({ page }) => {
  const responsePromise = page.waitForResponse((response) =>
    response
      .url()
      .includes(`governance-actions?search=&filters=&sort=newestFirst&page=2`)
  );
  const outcomePage = new OutComesPage(page);
  await outcomePage.goto();

  let governanceActionIdsBefore: String[];
  let governanceActionIdsAfter: String[];

  await functionWaitedAssert(
    async () => {
      governanceActionIdsBefore =
        await outcomePage.getAllListedCIP105GovernanceIds();
      await outcomePage.showMoreBtn.click();
    },
    { message: "Show more button not visible" }
  );

  const response = await responsePromise;
  const governanceActionListAfter = await response.json();

  await functionWaitedAssert(
    async () => {
      governanceActionIdsAfter =
        await outcomePage.getAllListedCIP105GovernanceIds();
      expect(governanceActionIdsAfter.length).toBeGreaterThan(
        governanceActionIdsBefore.length
      );
    },
    { message: "Outcomes not loaded after clicking show more" }
  );

  if (governanceActionListAfter.length >= governanceActionIdsBefore.length) {
    await expect(outcomePage.showMoreBtn).toBeVisible();
    expect(true).toBeTruthy();
  } else {
    await expect(outcomePage.showMoreBtn).not.toBeVisible();
  }
});

test("9G. Should display correct vote counts on outcome details page", async ({
  browser,
}) => {
  await Promise.all(
    Object.keys(outcomeType).map(async (filterKey) => {
      const page = await browser.newPage();
      injectLogger(page);
      const outcomeListResponsePromise = page.waitForResponse((response) =>
        response
          .url()
          .includes(`governance-actions?search=&filters=${filterKey}`)
      );
      const outcomePage = new OutComesPage(page);
      await outcomePage.goto(filterKey);

      const outcomeListResponse = await outcomeListResponsePromise;
      const proposals = await outcomeListResponse.json();

      expect(
        proposals.length,
        proposals.length == 0 && "No proposals found!"
      ).toBeGreaterThan(0);
      const {
        index: governanceActionIndex,
        tx_hash: governanceTransactionHash,
      } = proposals[0];

      const govActionDetailsPage = await outcomePage.viewFirstOutcomes();

      const outcomeResponse = await page.waitForResponse((response) =>
        response
          .url()
          .includes(
            `governance-actions/${governanceTransactionHash}?index=${governanceActionIndex}`
          )
      );
      const proposalToCheck = (await outcomeResponse.json())[0];

      const metricsResponse = await page.waitForResponse(
        (response) =>
          response.url().includes(`/network/metrics`) &&
          !response.url().includes(`/misc/network/metrics`)
      );

      const dRepTotalAbstainVote =
        await govActionDetailsPage.getDRepTotalAbstainVoted(
          proposalToCheck,
          metricsResponse
        );

      // check dRep votes
      if (await areDRepVoteTotalsDisplayed(proposalToCheck)) {
        await expect(govActionDetailsPage.dRepYesVotes).toHaveText(
          `₳ ${correctVoteAdaFormat(parseInt(proposalToCheck.yes_votes))}`
        );
        await expect(govActionDetailsPage.dRepAbstainVotes).toHaveText(
          `₳ ${correctVoteAdaFormat(dRepTotalAbstainVote)}`
        );
        await expect(govActionDetailsPage.dRepNoVotes).toHaveText(
          `₳ ${correctVoteAdaFormat(parseInt(proposalToCheck.no_votes))}`
        );
      }
      // check sPos votes
      if (await areSPOVoteTotalsDisplayed(proposalToCheck)) {
        await expect(govActionDetailsPage.sPosYesVotes).toHaveText(
          `₳ ${correctVoteAdaFormat(parseInt(proposalToCheck.pool_yes_votes))}`
        );
        await expect(govActionDetailsPage.sPosAbstainVotes).toHaveText(
          `₳ ${correctVoteAdaFormat(parseInt(proposalToCheck.pool_abstain_votes))}`
        );
        await expect(govActionDetailsPage.sPosNoVotes).toHaveText(
          `₳ ${correctVoteAdaFormat(parseInt(proposalToCheck.pool_no_votes))}`
        );
      }

      // check ccCommittee votes
      if (areCCVoteTotalsDisplayed(proposalToCheck)) {
        await expect(govActionDetailsPage.ccCommitteeYesVotes).toHaveText(
          `${proposalToCheck.cc_yes_votes}`
        );
        await expect(govActionDetailsPage.ccCommitteeAbstainVotes).toHaveText(
          `${proposalToCheck.cc_abstain_votes}`
        );
        await expect(govActionDetailsPage.ccCommitteeNoVotes).toHaveText(
          `${proposalToCheck.cc_no_votes}`
        );
      }
    })
  );
});
