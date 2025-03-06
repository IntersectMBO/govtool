import { correctVoteAdaFormat } from "@helpers/adaFormat";
import { setAllureEpic } from "@helpers/allure";
import { skipIfNotHardFork } from "@helpers/cardano";
import {
  areCCVoteTotalsDisplayed,
  areDRepVoteTotalsDisplayed,
  areSPOVoteTotalsDisplayed,
} from "@helpers/featureFlag";
import GovernanceActionDetailsPage from "@pages/governanceActionDetailsPage";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { expect } from "@playwright/test";
import { test } from "@fixtures/walletExtension";
import { GovernanceActionType, IProposal, PaginatedLiveProposal } from "@types";
import { injectLogger } from "@helpers/page";
import removeAllSpaces from "@helpers/removeAllSpaces";
import { functionWaitedAssert } from "@helpers/waitedLoop";
import extractExpiryDateFromText from "@helpers/extractExpiryDateFromText";
import { InvalidMetadata } from "@constants/index";

test.beforeEach(async () => {
  await setAllureEpic("4. Proposal visibility");
  await skipIfNotHardFork();
});

const infoTypeProposal: PaginatedLiveProposal = require("../../lib/_mock/infoTypeProposal.json");

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

test("4A_2. Should access Governance Actions page without connecting wallet", async ({
  page,
}) => {
  await page.goto("/");
  await page.getByTestId("move-to-governance-actions-button").click();

  await expect(page.getByText(/Governance actions/i)).toHaveCount(1);
});

test("4B_2. Should restrict voting for users who are not registered as DReps (without wallet connected)", async ({
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
    (p1, p2) => p1.dRepYesVotes >= p2.dRepYesVotes
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
    [removeAllSpaces(filterOptionNames[choice])]
  );
  await govActionsPage.validateFilters([filterOptionNames[choice]]);
});

test("4L. Should search governance actions", async ({ page }) => {
  let governanceActionId: string | undefined;
  await page.route("**/proposal/list?**", async (route) => {
    const response = await route.fetch();
    const data = await response.json();
    const elementsWithIds = data["elements"].map(
      (element) => element["txHash"] + "#" + element["index"]
    );
    if (elementsWithIds.length !== 0 && governanceActionId === undefined) {
      governanceActionId = elementsWithIds[0];
    }
    await route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify(data),
    });
  });
  const responsePromise = page.waitForResponse("**/proposal/list?**");

  const governanceActionPage = new GovernanceActionsPage(page);
  await governanceActionPage.goto();

  await responsePromise;

  await governanceActionPage.searchInput.fill(governanceActionId);

  await functionWaitedAssert(
    async () => {
      const proposalCards = await governanceActionPage.getAllProposals();

      for (const proposalCard of proposalCards) {
        await expect(
          proposalCard.getByTestId(`${governanceActionId}-id`)
        ).toBeVisible();
      }
    },
    { message: `${governanceActionId} not found` }
  );
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

test("4K. Should display correct vote counts on governance details page for disconnect state", async ({
  page,
  browser,
}) => {
  const responsesPromise = Object.keys(GovernanceActionType).map((filterKey) =>
    page.waitForResponse((response) =>
      response.url().includes(`&type[]=${GovernanceActionType[filterKey]}`)
    )
  );

  const metricsResponsePromise = page.waitForResponse((response) =>
    response.url().includes(`network/metrics`)
  );

  const governanceActionsPage = new GovernanceActionsPage(page);
  await governanceActionsPage.goto();
  const responses = await Promise.all(responsesPromise);
  const proposals: IProposal[] = (
    await Promise.all(
      responses.map(async (response) => {
        const data = await response.json();
        return data.elements;
      })
    )
  ).flat();

  expect(proposals.length, "No proposals found!").toBeGreaterThan(0);

  const uniqueProposalTypes = Array.from(
    new Map(proposals.map((proposal) => [proposal.type, proposal])).values()
  );

  await Promise.all(
    uniqueProposalTypes.map(async (proposalToCheck) => {
      const newPage = await browser.newPage();
      injectLogger(newPage);
      const govActionDetailsPage = new GovernanceActionDetailsPage(newPage);
      await govActionDetailsPage.goto(
        `${proposalToCheck.txHash}#${proposalToCheck.index}`
      );

      const dRepTotalAbstainVote =
        await govActionDetailsPage.getDRepTotalAbstainVoted(
          proposalToCheck,
          metricsResponsePromise
        );

      // check dRep votes
      if (await areDRepVoteTotalsDisplayed(proposalToCheck)) {
        await expect(govActionDetailsPage.dRepYesVotes).toHaveText(
          `₳ ${correctVoteAdaFormat(proposalToCheck.dRepYesVotes)}`
        );
        await expect(govActionDetailsPage.dRepAbstainVotes).toHaveText(
          `₳ ${correctVoteAdaFormat(dRepTotalAbstainVote)}`
        );
        await expect(govActionDetailsPage.dRepNoVotes).toHaveText(
          `₳ ${correctVoteAdaFormat(proposalToCheck.dRepNoVotes)}`
        );
      }
      // check sPos votes
      if (await areSPOVoteTotalsDisplayed(proposalToCheck)) {
        await expect(govActionDetailsPage.sPosYesVotes).toHaveText(
          `₳ ${correctVoteAdaFormat(proposalToCheck.poolYesVotes)}`
        );
        await expect(govActionDetailsPage.sPosAbstainVotes).toHaveText(
          `₳ ${correctVoteAdaFormat(proposalToCheck.poolAbstainVotes)}`
        );
        await expect(govActionDetailsPage.sPosNoVotes).toHaveText(
          `₳ ${correctVoteAdaFormat(proposalToCheck.poolNoVotes)}`
        );
      }

      // check ccCommittee votes
      if (areCCVoteTotalsDisplayed(proposalToCheck)) {
        await expect(govActionDetailsPage.ccCommitteeYesVotes).toHaveText(
          `${proposalToCheck.ccYesVotes}`
        );
        await expect(govActionDetailsPage.ccCommitteeAbstainVotes).toHaveText(
          `${proposalToCheck.ccAbstainVotes}`
        );
        await expect(govActionDetailsPage.ccCommitteeNoVotes).toHaveText(
          `${proposalToCheck.ccNoVotes}`
        );
      }
    })
  );
});

test.describe("Invalid Live voting Metadata", () => {
  InvalidMetadata.forEach(({ type, reason, url, hash }, index) => {
    test(`4P_${index + 1}: Should display ${type} message in live voting when ${reason}`, async ({
      page,
    }) => {
      const proposal: IProposal = {
        ...infoTypeProposal.elements[0],
        url,
        metadataHash: hash,
      };
      const liveProposalResponse: PaginatedLiveProposal = {
        ...infoTypeProposal,
        elements: [proposal],
      };

      await page.route("**/proposal/list?**", async (route) =>
        route.fulfill({
          body: JSON.stringify(liveProposalResponse),
        })
      );

      const governanceActionPage = new GovernanceActionsPage(page);
      await governanceActionPage.goto();
      await governanceActionPage.viewFirstProposal();

      await expect(page.getByRole("heading", { name: type })).toBeVisible({
        timeout: 60_000,
      });
      await expect(page.getByText("Learn more")).toBeVisible();
      await expect(page.getByTestId("external-modal-button")).toBeVisible();
    });
  });
});
