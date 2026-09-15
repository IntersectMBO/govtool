import { InvalidMetadata } from "@constants/index";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import OutcomeDetailsPage from "@pages/outcomeDetailsPage";
import OutComesPage from "@pages/outcomesPage";
import { Page } from "@playwright/test";

const invalidOutcomeProposals = require("../../lib/_mock/outcome.json");

test.beforeEach(async () => {
  await setAllureEpic("9. Outcomes");
});

test.describe("Outcomes page", () => {
  let outcomePage: OutComesPage;
  test.beforeEach(async ({ page }) => {
    outcomePage = new OutComesPage(page);
  });

  test("9A_1. Should access Outcomes page in disconnect state", async () => {
    await outcomePage.shouldAccessPage();
  });

  test.describe("outcome sorting and filtering", () => {
    test("9C_1A. Should filter Governance Action Type on governance actions page in disconnect state", async () => {
      test.slow();
      await outcomePage.goto();

      await outcomePage.filterOutcomes();
    });

    test("9C_2A. Should sort Governance Action Type on outcomes page in disconnect state", async () => {
      test.slow();

      await outcomePage.goto({ sort: "oldestFirst" });

      await outcomePage.sortOutcomes();
    });

    test("9C_3A. Should filter and sort Governance Action Type on outcomes page in disconnect state", async () => {
      await outcomePage.filterAndSortOutcomes();
    });
  });

  test("9E_1. Should verify all of the displayed governance actions have expired in disconnect state", async () => {
    await outcomePage.goto();

    await outcomePage.verifyAllOutcomesAreExpired();
  });

  test("9F_1. Should load more Outcomes on show more in disconnect state", async () => {
    await outcomePage.VerifyLoadMoreOutcomes();
  });

  test.describe("Outcome details dependent test", () => {
    let governanceActionId: string | undefined;
    let governanceActionTitle: string | undefined;
    let currentPage: Page;
    test.beforeEach(async ({ page }) => {
      const outcomePage = new OutComesPage(page);
      const response = await outcomePage.fetchOutcomeIdAndTitleFromNetwork(
        governanceActionId,
        governanceActionTitle
      );
      governanceActionId = response.governanceActionId;
      governanceActionTitle = response.governanceActionTitle;

      currentPage = page;
    });

    test("9B_1. Should search outcomes proposal by title and id in disconnect state", async () => {
      // search by id
      await outcomePage.searchOutcomesById(governanceActionId);

      await outcomePage.searchOutcomesByTitle(governanceActionTitle);
    });

    test("9D_1. Should copy governanceActionId in disconnect state", async ({
      context,
    }) => {
      await context.grantPermissions(["clipboard-read", "clipboard-write"]);
      await outcomePage.shouldCopyGovernanceActionId(governanceActionId);
    });
  });
});

test.describe("Outcome details", () => {
  test("9G_1. Should display correct vote counts on outcome details page in disconnect state", async ({
    browser,
    page,
  }) => {
    const outcomeDetailPage = new OutcomeDetailsPage(page);
    await outcomeDetailPage.shouldDisplayCorrectVotingResults(browser);
  });

  test.describe("Invalid Outcome Metadata", () => {
    InvalidMetadata.forEach(({ type, reason, url, hash }, index) => {
      test(`9H_${index + 1}A: Should display "${type}" message in outcomes when ${reason} in disconnect state`, async ({
        page,
      }) => {
        const outcomeResponse = {
          ...invalidOutcomeProposals[0],
          url,
          data_hash: hash,
        };

        const outcomeDetailPage = new OutcomeDetailsPage(page);
        await outcomeDetailPage.verifyInvalidOutcomeMetadata({
          outcomeResponse,
          type,
          url,
          hash,
        });
      });
    });
  });
});
