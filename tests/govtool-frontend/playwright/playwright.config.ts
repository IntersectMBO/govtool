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
  reporter: process.env.CI ? [["line"], ["allure-playwright"]] : [["line"]],
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
      name: "auth setup",
      testMatch: "**/auth.setup.ts",
    },
    {
      name: "dRep setup",
      testMatch: "**/dRep.setup.ts",
      dependencies: process.env.CI ? ["wallet bootstrap"] : [],
    },
    {
      name: "wallet bootstrap",
      testMatch: "**/wallet.bootstrap.ts",
    },
    {
      name: "transaction",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.tx.spec.ts",
      dependencies: process.env.CI ? ["auth setup", "wallet bootstrap"] : [],
    },
    {
      name: "logged in",
      use: { ...devices["Desktop Chrome"] },
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
      name: "independent",
      use: { ...devices["Desktop Chrome"] },
      testIgnore: [
        "**/*.tx.spec.ts",
        "**/*.loggedin.spec.ts",
        "**/*.dRep.spec.ts",
      ],
    },
    // {
    //   name: "cleanup adaHolder",
    //   testMatch: "**/*.teardown.ts",
    // },

    /* Test against mobile viewports. */
    // {
    //   name: 'Mobile Chrome',
    //   use: { ...devices['Pixel 5'] },
    // },
    // {
    //   name: 'Mobile Safari',
    //   use: { ...devices['iPhone 12'] },
    // },

    /* Test against branded browsers. */
    // {
    //   name: 'Microsoft Edge',
    //   use: { ...devices['Desktop Edge'], channel: 'msedge' },
    // },
    // {
    //   name: 'Google Chrome',
    //   use: { ...devices['Desktop Chrome'], channel: 'chrome' },
    // },
  ],

  /* Run your local dev server before starting the tests */
  // webServer: {
  //   command: "cd govtool/frontend && npm run start",
  //   // url: "http://127.0.0.1:3000",
  //   reuseExistingServer: !process.env.CI,
  // },
});
