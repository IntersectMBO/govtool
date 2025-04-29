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
      name: "adaholder auth setup",
      testMatch: "**/adaholder.auth.setup.ts",
    },
    {
      name: "user auth setup",
      testMatch: "**/user.auth.setup.ts",
    },
    {
      name: "faucet setup",
      testMatch: "**/faucet.setup.ts",
      teardown: environments.ci && "cleanup faucet",
    },
    {
      name: "dRep setup",
      testMatch: "**/dRep.setup.ts",
      dependencies: environments.ci ? ["faucet setup", "wallet bootstrap"] : [],
    },
    {
      name: "proposal budget dRep setup",
      testMatch: "**/proposal-budget.dRep.setup.ts",
      teardown: environments.ci && "cleanup faucet",
    },
    {
      name: "dRep auth setup",
      testMatch: "**/dRep.auth.setup.ts",
      dependencies: environments.ci ? ["dRep setup"] : [],
    },
    {
      name: "proposal discussion auth setup",
      testMatch: "**/proposal-discussion.auth.setup.ts",
    },
    {
      name: "proposal budget auth setup",
      testMatch: "**/proposal-budget.auth.setup.ts",
    },
    {
      name: "proposal setup",
      testMatch: "**/proposal.setup.ts",
      teardown: environments.ci && "cleanup faucet",
    },
    {
      name: "wallet bootstrap",
      testMatch: "**/wallet.bootstrap.ts",
      dependencies: environments.ci ? ["faucet setup"] : [],
    },
    {
      name: "transaction",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.tx.spec.ts",
      dependencies: environments.ci ? ["adaholder auth setup", "wallet bootstrap"] : [],
    },
    {
      name: "proposal discussion",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.pd.spec.ts",
      dependencies: environments.ci
        ? ["proposal discussion auth setup"]
        : [],
    },
    {
      name: "proposal budget",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.pb.spec.ts",
      dependencies: environments.ci
        ? ["proposal budget auth setup"]
        : [],
      testIgnore: ["**/*.dRep.pb.spec.ts"],
    },
    {
      name: "proposal submission",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.ga.spec.ts",
      dependencies: environments.ci
        ? ["proposal setup"]
        : [],
    },
    {
      name: "loggedin (desktop)",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.loggedin.spec.ts",
      dependencies: environments.ci ? ["user auth setup"] : [],
    },
    {
      name: "proposal budget dRep",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.dRep.pb.spec.ts",
      dependencies: environments.ci
        ? ["proposal budget auth setup","proposal budget dRep setup"]
        : [],
      teardown: environments.ci && "cleanup dRep",
    },
    {
      name: "dRep",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.dRep.spec.ts",
      dependencies: environments.ci
        ? ["dRep setup", "dRep auth setup",  "wallet bootstrap"]
        : [],
      teardown: environments.ci && "cleanup dRep",
    },
    {
      name: "delegation",
      use: { ...devices["Desktop Chrome"] },
      testMatch: "**/*.delegation.spec.ts",
      dependencies: environments.ci
        ? ["adaholder auth setup","dRep auth setup", "dRep setup", "wallet bootstrap"]
        : [],
      teardown: environments.ci && "cleanup delegation",
    },
    {
      name: "independent (desktop)",
      use: { ...devices["Desktop Chrome"] },
      testIgnore: [
        "**/*.delegation.spec.ts",
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
        "**/*.loggedin.spec.ts",
        "**/*.dRep.spec.ts",
        "**/*.delegation.spec.ts",
        "**/*.tx.spec.ts",
        "**/*.ga.spec.ts",
        "**/*.pd.spec.ts",
        "**/*.pb.spec.ts",
        "**/walletConnect.spec.ts",
      ],
    },
    {
      name: "cleanup delegation",
      testMatch: "delegation.teardown.ts",
    },
    {
      name: "cleanup dRep",
      testMatch: "dRep.teardown.ts",
    },
    {
      name: "cleanup faucet",
      testMatch: "faucet.teardown.ts",
    },
  ],
});
