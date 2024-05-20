import { defineConfig, devices } from "@playwright/test";
import { testPlanFilter } from "allure-playwright/dist/testplan";
import { config } from "dotenv";
import environments from "lib/constants/environments";

config();

/**
 * Read environment variables from file.
 * https://github.com/motdotla/dotenv
 */
// require('dotenv').config();

/**
 * See https://playwright.dev/docs/test-configuration.
 */
export default defineConfig({
  testDir: "./tests",
  /* Run tests in files in parallel */
  fullyParallel: true,
  /* Fail the build on CI if you accidentally left test.only in the source code. */
  forbidOnly: !!process.env.CI,
  /* Retry on CI only */
  retries: 0,
  /* Opt out of parallel tests on CI. */
  workers: process.env.CI ? parseInt(process.env.TEST_WORKERS) : undefined,
  /*use Allure Playwright's testPlanFilter() to determine the grep parameter*/
  grep: testPlanFilter(),
  /* Reporter to use. See https://playwright.dev/docs/test-reporters */
  reporter: process.env.CI
    ? [
        ["line"],
        [
          "allure-playwright"
        ],
      ]
    : [["line"]],
  /* Shared settings for all the projects below. See https://playwright.dev/docs/api/class-testoptions. */
  use: {
    /* Base URL to use in actions like `await page.goto('/')`. */
    baseURL: environments.frontendUrl,

    /* Collect trace when retrying the failed test. See https://playwright.dev/docs/trace-viewer */
    trace: "on-first-retry",

    screenshot: "only-on-failure",

    // video: "on",
  },

  /* Configure projects for major browsers */
  projects: [
    {
      name: "faucet setup",
      testMatch: "**/faucet.setup.ts",
    },
    {
      name: "auth setup",
      testMatch: "**/auth.setup.ts",
    },
    {
      name: "dRep setup",
      testMatch: "**/dRep.setup.ts",
      dependencies: ["faucet setup"],
    },
    {
      name: "wallet bootstrap",
      testMatch: "**/wallet.bootstrap.ts",
      dependencies: ["faucet setup"],
    },
    // {
    //   name: "transaction",
    //   use: { ...devices["Desktop Chrome"] },
    //   testMatch: "**/*.tx.spec.ts",
    //   dependencies: process.env.CI ? ["auth setup", "wallet bootstrap"] : [],
    // },
    {
      name: "loggedin (desktop)",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.loggedin.spec.ts",
      dependencies: process.env.CI ? ["auth setup"] : [],
    },
    {
      name: "loggedin (mobile)",
      use: { ...devices["Pixel 5"] },
      testMatch: "**/*.loggedin.spec.ts",
      dependencies: process.env.CI ? ["auth setup"] : [],
    },
    {
      name: "dRep",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.dRep.spec.ts",
      dependencies: process.env.CI ? ["auth setup", "dRep setup"] : [],
    },
    {
      name: "delegation",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.delegation.spec.ts",
      dependencies: process.env.CI
        ? ["auth setup", "dRep setup", "wallet bootstrap"]
        : [],
      teardown: process.env.CI && "cleanup delegation",
    },
    {
      name: "independent (desktop)",
      use: { ...devices["Desktop Chrome"] },
      testIgnore: [
        "**/*.delegation.spec.ts",
        "**/*.loggedin.spec.ts",
        "**/*.dRep.spec.ts",
      ],
    },
    {
      name: "independent (mobile)",
      use: { ...devices["Pixel 5"] },
      testIgnore: [
        "**/*.loggedin.spec.ts",
        "**/*.dRep.spec.ts",
        "**/*.delegation.spec.ts",
      ],
    },
    {
      name: "cleanup delegation",
      testMatch: "delegation.teardown.ts",
    },
  ],
});
