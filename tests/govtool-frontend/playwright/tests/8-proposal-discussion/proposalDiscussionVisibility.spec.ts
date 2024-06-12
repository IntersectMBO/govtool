import { faker } from "@faker-js/faker";
import { test } from "@fixtures/proposalDiscussionDetailsPage";
import ProposalDiscussionPage from "@pages/proposalDiscussionPage";
import { expect } from "@playwright/test";
import proposalManager from "lib/proposalManager";

test("8B. Should filter and sort the list of proposed governance actions.", async ({
  page,
}) => {
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  await proposalDiscussionPage.filterBtn.click();
  await proposalDiscussionPage.infoRadio.click();

  await expect(page.getByText("Treasury")).toHaveCount(1);

  await proposalDiscussionPage.treasuryRadio.click();
});


test("8C. Should search the list of proposed governance actions.", async ({
  page,
}) => {
  const proposalPayload = await proposalManager.getProposalPayload();
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  await proposalDiscussionPage.searchInput.fill(proposalPayload.prop_name);

  const proposalCards = await proposalDiscussionPage.getAllProposals();

  for (const proposalCard of proposalCards) {
    expect(
      (await proposalCard.textContent())
        .toLowerCase()
        .includes(`${proposalPayload.prop_name}`)
    ).toBeTruthy();
  }
});


test("8D.Should show the view-all categorized proposed governance actions.", async ({
  page,
}) => {
  const proposalDiscussionPage = new ProposalDiscussionPage(page);
  await proposalDiscussionPage.goto();

  await proposalDiscussionPage.showAllBtn.click();

  await expect(proposalDiscussionPage.showLessBtn).toBeVisible();
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

test("8I. Should disable poll voting functionality.", async ({
  proposalDiscussionDetailsPage,
}) => {
  await proposalDiscussionDetailsPage.goto();

  await expect(proposalDiscussionDetailsPage.pollVoteCard).not.toBeVisible();
  await expect(proposalDiscussionDetailsPage.pollYesVoteCount).not.toBeVisible();

  await expect(proposalDiscussionDetailsPage.pollNoVoteCount).not.toBeVisible();
   
});
