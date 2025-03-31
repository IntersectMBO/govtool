import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { skipTestForProposalBudget } from "@helpers/cardano";

test.beforeEach(async ({}) => {
  await setAllureEpic("12. Proposal Budget Submission");
  await skipTestForProposalBudget();
});

test("12B. Should access proposal creation page in connected state", async ({}) => {});

test("12C. Should view draft proposal", async ({}) => {});

test.describe("Budget proposal field verification", () => {
  test("12D_1. Should verify all field of “contact information” section", async ({}) => {});
  test("12D_2. Should verify all field of “proposal ownership” section", async ({}) => {});
  test("12D_3. Should verify all field of “problem statements and proposal benefits” section", async ({}) => {});
  test("12D_4. Should verify all field of “costing” section", async ({}) => {});
  test("12D_5. Should verify all field of “further information” section", async ({}) => {});
  test("12D_6. Should verify all field of “administration and auditing” section", async ({}) => {});
  test("12D_7. Should verify all field of “submit” section", async ({}) => {});
});

test.describe("Budget proposal field validation", () => {
  test("12E_1. Should accept valid data in “contact information” section", async ({}) => {});
  test("12E_2. Should accept valid data in “proposal ownership” section", async ({}) => {});
  test("12E_3. Should accept valid data in “problem statements and proposal benefits” section", async ({}) => {});
  test("12E_4. Should accept valid data in “costing” section", async ({}) => {});
  test("12E_5. Should accept valid data in “further information” section", async ({}) => {});
  test("12E_6. Should accept valid data in “administration and auditing” section", async ({}) => {});
  test("12E_7. Should accept valid data in “submit” section", async ({}) => {});
  test("12F_1. Should reject invalid data in “contact information” section", async ({}) => {});
  test("12F_2. Should reject invalid data in “proposal ownership” section", async ({}) => {});
  test("12F_3. Should reject invalid data in “problem statements and proposal benefits” section", async ({}) => {});
  test("12F_4. Should reject invalid data in “costing” section", async ({}) => {});
  test("12F_5. Should reject invalid data in “further information” section", async ({}) => {});
  test("12F_6. Should reject invalid data in “administration and auditing” section", async ({}) => {});
  test("12F_7. Should reject invalid data in “submit” section", async ({}) => {});
});

test("12G. Should validate and review submitted budget proposal", async ({}) => {});
test("12H. Should save a budget proposal as a draft", async ({}) => {});

test("12I. Should submit a valid budget proposal", async ({}) => {});
test("12J. Should submit a valid draft budget proposal", async ({}) => {});
