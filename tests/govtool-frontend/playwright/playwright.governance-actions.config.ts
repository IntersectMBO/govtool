import { defineConfig, devices } from "@playwright/test";
import path from "node:path";

// Isolated display checks: no wallets, faucet, registration or transaction submission.
export default defineConfig({
  testDir: "./tests/9-governance-action-history",
  testMatch: "governance-actions.aggregates.ui.spec.ts",
  workers: 2,
  timeout: 60_000,
  expect: { timeout: 15_000 },
  reporter: "list",
  outputDir: "./test-results/governance-actions-aggregates",
  use: { baseURL: "http://127.0.0.1:4178", screenshot: "only-on-failure" },
  projects: [
    { name: "governance-actions-desktop", use: { ...devices["Desktop Chrome"] } },
    { name: "governance-actions-mobile", use: { ...devices["Pixel 5"] } },
  ],
  webServer: {
    command: "npm run dev -- --host 127.0.0.1 --port 4178 --strictPort",
    cwd: path.resolve(__dirname, "../../../govtool/frontend"),
    url: "http://127.0.0.1:4178",
    timeout: 120_000,
    env: {
      VITE_BASE_URL: "/fixture-api",
    },
  },
});
