import { defineConfig, devices } from "@playwright/test";
import path from "node:path";

// Isolated from the funded-wallet suite: no API keys, faucet or chain submission.
export default defineConfig({
  testDir: "./tests/cip179",
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: 0,
  workers: 2,
  timeout: 60_000,
  expect: { timeout: 15_000 },
  reporter: "list",
  outputDir: "./test-results/cip179",
  use: {
    baseURL: "http://127.0.0.1:4179",
    trace: "retain-on-failure",
    screenshot: "only-on-failure",
    launchOptions: {
      executablePath: process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH,
    },
  },
  projects: [
    { name: "cip179-desktop", use: { ...devices["Desktop Chrome"] } },
    { name: "cip179-mobile", use: { ...devices["Pixel 5"] } },
  ],
  webServer: {
    command: "npm run dev -- --host 127.0.0.1 --port 4179 --strictPort",
    cwd: path.resolve(__dirname, "../../../govtool/frontend"),
    url: "http://127.0.0.1:4179",
    timeout: 120_000,
    reuseExistingServer: false,
    env: { VITE_IS_CIP179_ENABLED: "" },
  },
});
