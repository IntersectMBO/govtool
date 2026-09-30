import { expect } from "@playwright/test";
import { Logger } from "./logger";

export async function waitedLoop(
  conditionFn,
  timeout = 60000,
  interval = 2000
) {
  const startTime = Date.now();
  while (Date.now() - startTime < timeout) {
    if (await conditionFn()) return true;
    Logger.info("Retrying the function");
    await new Promise((resolve) => setTimeout(resolve, interval));
  }
  return false;
}

export async function functionWaitedAssert(
  fn,
  options: {
    timeout?: number;
    interval?: number;
    message?: string;
    name?: string;
  } = {}
) {
  const startTime = Date.now();
  const timeout = options.timeout || 60000;
  const interval = options.interval || 2000;
  const name = options.name || "";

  while (true) {
    try {
      await fn();
      return true;
    } catch (error) {
      if (Date.now() - startTime >= timeout) {
        // Callers throw strings as well as Errors; keep either's text.
        const errorMessage =
          options.message || (error instanceof Error ? error.message : String(error));
        console.log(errorMessage);
        expect(false, { message: errorMessage }).toBe(true);
      }
      Logger.info(`Retrying the function ${name}`);
      await new Promise((resolve) => setTimeout(resolve, interval));
    }
  }
}
