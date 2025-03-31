import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipTestForProposalBudget } from "@helpers/cardano";

test.beforeEach(async ({}) => {
  await setAllureEpic("11. Proposal Budget");
  await skipTestForProposalBudget();
});

test("11H. Should restrict non registered DRep users from voting", async () => {});

test("11I. Should comments on any proposal", async ({}) => {});

test("11J. Should reply to any comments", async ({}) => {});
