import { defineConfig, devices } from "@playwright/test";
import { testPlanFilter } from "allure-playwright/dist/testplan";
import environments from "lib/constants/environments";

/**
 * Read environment variables from file.
 * https://github.com/motdotla/dotenv
 */

/**
 * See https://playwright.dev/docs/test-configuration.
 */
export default defineConfig({
  testDir: "./tests",
  // Picks this run's HD account base for the test wallets.
  globalSetup: "./lib/wallet/hdRunSetup.ts",
  // Run these deterministic local tests with npm run test:cip179.
  testIgnore: ["**/cip179/**"],
  /* Run tests in files in parallel */
  fullyParallel: true,
  /**TODO: Remove this timeout *
   * It has been intentionally used to slow loading of govtool.
   */
  timeout: process.env.NETWORK === "preview" ? 180_000 : 90_000,
  /* Fail the build on CI if you accidentally left test.only in the source code. */
  forbidOnly: !!environments.ci,
  /* Retry on CI only */
  retries: 0,
  /* Opt out of parallel tests on CI. */
  workers: environments.ci ? parseInt(process.env.TEST_WORKERS) : undefined,
  /*use Allure Playwright's testPlanFilter() to determine the grep parameter*/
  grep: testPlanFilter(),
  /* Reporter to use. See https://playwright.dev/docs/test-reporters */
  reporter: environments.ci ? [["line"], ["allure-playwright"]] : [["line"]],
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
      // Registers the shared DReps (dRep01..03) that other tests delegate to
      // or look up; tests also register them on first use.
      name: "dRep setup",
      testMatch: "**/dRep.setup.ts",
    },
    {
      name: "proposal discussion",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.pd.spec.ts",
      testIgnore: ["**/*.loggedin.pd.spec.ts"],
    },
    {
      name: "proposal discussion (loggedin)",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.loggedin.pd.spec.ts",
    },
    {
      name: "budget proposal",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.pb.spec.ts",
      testIgnore: ["**/*.dRep.pb.spec.ts"],
    },
    {
      name: "proposal submission",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.ga.spec.ts",
    },
    {
      name: "loggedin (desktop)",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.loggedin.spec.ts",
    },
    {
      name: "budget proposal dRep",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.dRep.pb.spec.ts",
      dependencies: environments.ci ? ["dRep setup"] : [],
    },
    {
      name: "dRep",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.dRep.spec.ts",
      dependencies: environments.ci ? ["dRep setup"] : [],
    },
    {
      name: "delegation",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.delegation.spec.ts",
      dependencies: environments.ci ? ["dRep setup"] : [],
    },
    {
      // Wallets derived from TEST_WALLET_MNEMONIC and funded on demand.
      name: "wallet",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.wallet.spec.ts",
    },
    {
      name: "independent (desktop)",
      use: { ...devices["Desktop Chrome"] },
      testIgnore: [
        "**/cip179/**",
        "**/*.delegation.spec.ts",
        "**/*.wallet.spec.ts",
        "**/*.loggedin.spec.ts",
        "**/*.dRep.spec.ts",
        "**/*.tx.spec.ts",
        "**/*.ga.spec.ts",
        "**/*.pd.spec.ts",
        "**/*.pb.spec.ts",
      ],
    },
    {
      name: "mobile",
      use: { ...devices["Pixel 5"] },
      testIgnore: [
        "**/cip179/**",
        "**/*.loggedin.spec.ts",
        "**/*.dRep.spec.ts",
        "**/*.delegation.spec.ts",
        "**/*.wallet.spec.ts",
        "**/*.tx.spec.ts",
        "**/*.ga.spec.ts",
        "**/*.pd.spec.ts",
        "**/*.pb.spec.ts",
        "**/walletConnect.spec.ts",
      ],
    },
  ],
});
