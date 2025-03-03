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
import { GovernanceActionType, IProposal } from "@types";
import { injectLogger } from "@helpers/page";

test.beforeEach(async () => {
  await setAllureEpic("4. Proposal visibility");
  await skipIfNotHardFork();
});

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
