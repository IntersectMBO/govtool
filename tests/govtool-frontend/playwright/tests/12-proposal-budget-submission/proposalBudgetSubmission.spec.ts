import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipTestForProposalBudget } from "@helpers/cardano";

test.beforeEach(async ({}) => {
  await setAllureEpic("12. Proposal Budget Submission");
  await skipTestForProposalBudget();
});

test("12A. Should restrict from creating a budget proposal in disconnect state", async ({
  page,
}) => {});
