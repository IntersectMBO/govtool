import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipTestForProposalBudget } from "@helpers/cardano";

test.beforeEach(async ({}) => {
  await setAllureEpic("11. Proposal Budget");
  await skipTestForProposalBudget();
});

test("11A. Should access budget proposal page", async ({}) => {});

test.describe("Budget proposal list manipulation", () => {
  test("11B_1. Should search for budget proposals by title", async ({}) => {});

  test("11B_2. Should filter budget proposals by categories", async ({}) => {});

  test("11B_3. Should sort budget proposals", async ({}) => {});
});

test("11C. Should show view-all categorized budget proposal", async ({}) => {});

test("11D. Should share budget proposal", async ({}) => {});

test("11E. Should view comments with count indications on a budget proposal", async () => {});

test.describe("Restricted access to interact budget proposal", () => {
  test("11F_1. Should restrict users without wallets from commenting", async () => {});
  test("11F_2. Should restrict users without wallets from voting", async () => {});
});

test("11G. Should sort the budget proposal comments", async ({}) => {});
