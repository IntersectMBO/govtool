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

export async function failureWithConsoleMessages(page: Page, fn: Function) {
  const consoleMessages: string[] = [];
  page.on("console", (msg) => {
    if (msg.type() === "error") {
      consoleMessages.push(msg.text());
    }
  });
  try {
    await fn();
  } catch (error) {
    Logger.fail(
      `Failed: ${error.message}\nConsole messages: ${consoleMessages.join("\n")}`
    );
    throw error;
  }
}
