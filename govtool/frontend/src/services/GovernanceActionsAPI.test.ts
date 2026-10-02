import { afterEach, describe, expect, it, vi } from "vitest";

afterEach(() => {
  vi.unstubAllEnvs();
  vi.resetModules();
});

describe("GovTool governance action routing", () => {
  it.each([
    ["http://localhost:9999", "http://localhost:9999"],
    ["https://govtool.example/api/", "https://govtool.example/api"],
    ["/api", "/api"],
    [undefined, ""],
  ])("uses the configured backend %s", async (backend, expected) => {
    vi.stubEnv("VITE_BASE_URL", backend);

    const { GovernanceActionsAPI } = await import("./GovernanceActionsAPI");

    expect(GovernanceActionsAPI.defaults.baseURL).toBe(expected);
  });
});
