import { setAllureEpic } from "@helpers/allure";
import { lovelaceToAda } from "@helpers/cardano";
import GovernanceActionsPage from "@pages/governanceActionsPage";
import { expect, test } from "@playwright/test";
import { FilterOption, IProposal } from "@types";

test.beforeEach(async () => {
  await setAllureEpic("4. Proposal visibility");
});

test("4A.2. Should access Governance Actions page without connecting wallet", async ({
  page,
}) => {
  await page.goto("/");
  await page.getByTestId("move-to-governance-actions-button").click();

  await expect(page.getByText(/Governance actions/i)).toHaveCount(2);
});

test("4B.2. Should restrict voting for users who are not registered as DReps (without wallet connected)", async ({
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
      .getByText(`₳ ${lovelaceToAda(proposalToCheck.yesVotes)}`)
  ).toBeVisible();
  await expect(
    page
      .getByText("abstain₳")
      .getByText(`₳ ${lovelaceToAda(proposalToCheck.abstainVotes)}`)
  ).toBeVisible();
  await expect(
    page
      .getByText("no₳")
      .getByText(`₳ ${lovelaceToAda(proposalToCheck.noVotes)}`)
  ).toBeVisible();
});
