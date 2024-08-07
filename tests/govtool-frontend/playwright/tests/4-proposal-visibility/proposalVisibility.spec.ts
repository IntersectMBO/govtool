import { setAllureEpic } from "@helpers/allure";
import { lovelaceToAda, skipIfNotHardFork } from "@helpers/cardano";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { expect, test } from "@playwright/test";
import { FilterOption, IProposal } from "@types";

test.beforeEach(async () => {
  await setAllureEpic("4. Proposal visibility");
  await skipIfNotHardFork();
});

test("4A_2. Should access Governance Actions page without connecting wallet", async ({
  page,
}) => {
  await page.goto("/");
  await page.getByTestId("move-to-governance-actions-button").click();

  await expect(page.getByText(/Governance actions/i)).toHaveCount(2);
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
}) => {
  const responsesPromise = Object.keys(FilterOption).map((filterKey) =>
    page.waitForResponse((response) =>
      response.url().includes(`&type[]=${FilterOption[filterKey]}`)
    )
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

  const proposalToCheck = proposals[0];
  await governanceActionsPage.viewProposal(proposalToCheck);

  await expect(
    page
      .getByText("yes₳")
      .first()
      .getByText(`₳ ${lovelaceToAda(proposalToCheck.dRepYesVotes)}`)
  ).toBeVisible();
  await expect(
    page
      .getByText("abstain₳")
      .first()
      .getByText(`₳ ${lovelaceToAda(proposalToCheck.dRepAbstainVotes)}`)
  ).toBeVisible();
  await expect(
    page
      .getByText("no₳")
      .first()
      .getByText(`₳ ${lovelaceToAda(proposalToCheck.dRepNoVotes)}`)
  ).toBeVisible();
  await expect(
    page
      .getByText("yes₳")
      .nth(1)
      .getByText(`₳ ${lovelaceToAda(proposalToCheck.poolYesVotes)}`)
  ).toBeVisible();
  await expect(
    page
      .getByText("abstain₳")
      .nth(1)
      .getByText(`₳ ${lovelaceToAda(proposalToCheck.poolAbstainVotes)}`)
  ).toBeVisible();
  await expect(
    page
      .getByText("no₳")
      .nth(1)
      .getByText(`₳ ${lovelaceToAda(proposalToCheck.poolNoVotes)}`)
  ).toBeVisible();
  await expect(
    page
      .getByText("yes₳")
      .nth(2)
      .getByText(`₳ ${lovelaceToAda(proposalToCheck.ccYesVotes)}`)
  ).toBeVisible();
  await expect(
    page
      .getByText("abstain₳")
      .nth(2)
      .getByText(`₳ ${lovelaceToAda(proposalToCheck.ccAbstainVotes)}`)
  ).toBeVisible();
  await expect(
    page
      .getByText("no₳")
      .nth(2)
      .getByText(`₳ ${lovelaceToAda(proposalToCheck.ccNoVotes)}`)
  ).toBeVisible();
});
