import { Expect, ExpectMatcherUtils } from "@playwright/test";

export async function expectWithInfo(
  expectation: () => Promise<void>,
  errorMessage: string
) {
  try {
    await expectation();
  } catch (e) {
    throw new Error(errorMessage);
  }
}
