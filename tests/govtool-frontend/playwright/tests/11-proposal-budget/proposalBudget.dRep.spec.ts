import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipTestForProposalBudget } from "@helpers/cardano";

test.beforeEach(async ({}) => {
  await setAllureEpic("11. Proposal Budget");
  await skipTestForProposalBudget();
});

test("11K. Should allow registered DRep to vote on a proposal", async ({}) => {});
test("11L. Should display DRep tag, name and ID when a registered DRep comments on a proposal", async ({}) => {});
