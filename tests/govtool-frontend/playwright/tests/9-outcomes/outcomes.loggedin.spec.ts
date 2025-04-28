import { user01AuthFile } from "@constants/auth";
import { InvalidMetadata } from "@constants/index";
import { user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import OutcomeDetailsPage from "@pages/outcomeDetailsPage";
import OutComesPage from "@pages/outcomesPage";
import { Page } from "@playwright/test";

const invalidOutcomeProposals = require("../../lib/_mock/outcome.json");

test.beforeEach(async () => {
  await setAllureEpic("9. Outcomes");
});

test.use({
  storageState: user01AuthFile,
  wallet: user01Wallet,
});

test.describe("Outcomes page", () => {
  let outcomePage: OutComesPage;
  test.beforeEach(async ({ page }) => {
    outcomePage = new OutComesPage(page);
  });

  test("9A_2. Should access Outcomes page in connected state", async () => {
    await outcomePage.shouldAccessPage();
  });
  test.describe("outcome sorting and filtering", () => {
    test("9C_1B. Should filter Governance Action Type on governance actions page", async () => {
      test.slow();
      await outcomePage.goto();

      await outcomePage.filterOutcomes();
    });

    test("9C_2B. Should sort Governance Action Type on outcomes page", async () => {
      test.slow();

      await outcomePage.goto({ sort: "oldestFirst" });

      await outcomePage.sortOutcomes();
    });

    test("9C_3B. Should filter and sort Governance Action Type on outcomes page", async () => {
      await outcomePage.filterAndSortOutcomes();
    });
  });

  test("9E_2. Should verify all of the displayed governance actions have expired", async () => {
    await outcomePage.goto();

    await outcomePage.verifyAllOutcomesAreExpired();
  });

  test("9F_2. Should load more Outcomes on show more", async () => {
    await outcomePage.VerifyLoadMoreOutcomes();
  });

  test.describe("Outcome details dependent test", () => {
    let governanceActionId: string | undefined;
    let governanceActionTitle: string | undefined;
    let currentPage: Page;
    test.beforeEach(async ({ page }) => {
      const outcomePage = new OutComesPage(page);
      governanceActionId =
        await outcomePage.fetchOutcomeIdFromNetwork(governanceActionId);
      governanceActionTitle = await outcomePage.fetchOutcomeTitleFromNetwork(
        governanceActionTitle
      );
      currentPage = page;
    });

    test("9B_2. Should search outcomes proposal by title and id", async () => {
      // search by id
      await outcomePage.searchOutcomesById(governanceActionId);

      await outcomePage.searchOutcomesByTitle(governanceActionTitle);
    });

    test("9D_2. Should copy governanceActionId in disconnect state", async ({
      context,
    }) => {
      await context.grantPermissions(["clipboard-read", "clipboard-write"]);
      await outcomePage.shouldCopyGovernanceActionId(governanceActionId);
    });
  });
});

test.describe("Outcome details", () => {
  test("9G_2. Should display correct vote counts on outcome details page", async ({
    browser,
    page,
  }) => {
    const outcomeDetailPage = new OutcomeDetailsPage(page);

    await outcomeDetailPage.shouldDisplayCorrectVotingResults(browser, true);
  });

  test.describe("Invalid Outcome Metadata", () => {
    InvalidMetadata.forEach(({ type, reason, url, hash }, index) => {
      test(`9H_${index + 1}B: Should display "${type}" message in outcomes when ${reason}`, async ({
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
