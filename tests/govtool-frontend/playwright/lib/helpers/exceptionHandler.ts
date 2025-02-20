import { Page } from "@playwright/test";
import { Logger } from "./logger";

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
