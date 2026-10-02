import { InvalidMetadata } from "@constants/index";
import { ensureInvalidMetadataFixtures } from "@helpers/invalidMetadataFixtures";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import GovernanceActionHistoryDetailsPage from "@pages/governanceActionHistoryDetailsPage";
import GovernanceActionHistoryPage from "@pages/governanceActionHistoryPage";
import { Page } from "@playwright/test";

const invalidGovernanceActionProposals = require("../../lib/_mock/governance-action.json");

test.beforeEach(async () => {
  await setAllureEpic("9. Governance action history");
});

test.describe("Governance action history page", () => {
  let actionRecordPage: GovernanceActionHistoryPage;
  test.beforeEach(async ({ page }) => {
    actionRecordPage = new GovernanceActionHistoryPage(page);
  });

  test("9A_1. Should access governance action history page in disconnect state", async () => {
    await actionRecordPage.shouldAccessPage();
  });

  test.describe("actionRecord sorting and filtering", () => {
    test("9C_1A. Should filter Governance Action Type on governance actions page in disconnect state", async () => {
      test.slow();
      await actionRecordPage.goto();

      await actionRecordPage.filterGovernanceActionHistory();
    });

    test("9C_2A. Should sort Governance Action Type on governance action history page in disconnect state", async () => {
      test.slow();

      await actionRecordPage.goto({ sort: "oldestFirst" });

      await actionRecordPage.sortGovernanceActionHistory();
    });

    test("9C_3A. Should filter and sort Governance Action Type on governance action history page in disconnect state", async () => {
      await actionRecordPage.filterAndSortGovernanceActionHistory();
    });
  });

  test("9E_1. Should verify all of the displayed governance actions have expired in disconnect state", async () => {
    await actionRecordPage.verifyAllGovernanceActionHistoryAreExpired();
  });

  test("9F_1. Should load more governance actions on show more in disconnect state", async () => {
    await actionRecordPage.VerifyLoadMoreGovernanceActionHistory();
  });

  test.describe("GovernanceAction details dependent test", () => {
    let governanceActionId: string | undefined;
    let governanceActionTitle: string | undefined;
    let currentPage: Page;
    test.beforeEach(async ({ page }) => {
      const actionRecordPage = new GovernanceActionHistoryPage(page);
      const response = await actionRecordPage.fetchGovernanceActionIdAndTitleFromNetwork(
        governanceActionId,
        governanceActionTitle
      );
      governanceActionId = response.governanceActionId;
      governanceActionTitle = response.governanceActionTitle;

      currentPage = page;
    });

    test("9B_1. Should search governanceActionHistory proposal by title and id in disconnect state", async () => {
      // search by id
      await actionRecordPage.searchGovernanceActionHistoryById(governanceActionId);

      await actionRecordPage.searchGovernanceActionHistoryByTitle(governanceActionTitle);
    });

    test("9D_1. Should copy governanceActionId in disconnect state", async ({
      context,
    }) => {
      await context.grantPermissions(["clipboard-read", "clipboard-write"]);
      await actionRecordPage.shouldCopyGovernanceActionId(governanceActionId);
    });
  });
});

test.describe("GovernanceAction details", () => {
  test("9G_1. Should display correct vote counts on actionRecord details page in disconnect state", async ({
    browser,
    page,
  }) => {
    const actionRecordDetailPage = new GovernanceActionHistoryDetailsPage(page);
    await actionRecordDetailPage.shouldDisplayCorrectVotingResults(browser);
  });

  test.describe("Invalid GovernanceAction Metadata", () => {
    test.beforeAll(async () => {
      await ensureInvalidMetadataFixtures();
    });

    InvalidMetadata.forEach(({ type, reason, url, hash }, index) => {
      test(`9H_${index + 1}A: Should display "${type}" message in governanceActionHistory when ${reason} in disconnect state`, async ({
        page,
      }) => {
        const actionRecordResponse = {
          ...invalidGovernanceActionProposals[0],
          url,
          data_hash: hash,
        };

        const actionRecordDetailPage = new GovernanceActionHistoryDetailsPage(page);
        await actionRecordDetailPage.verifyInvalidGovernanceActionMetadata({
          actionRecordResponse,
          type,
          url,
          hash,
        });
      });
    });
  });
});
