import { afterEach, describe, expect, it, vi } from "vitest";

afterEach(() => {
  vi.unstubAllEnvs();
  vi.resetModules();
});

describe("OutcomesAPI routing", () => {
  it.each([
    ["http://localhost:9999", "http://localhost:9999/outcomes"],
    ["https://govtool.example/api/", "https://govtool.example/api/outcomes"],
    ["/api", "/api/outcomes"],
    [undefined, "/outcomes"],
  ])("uses GovTool backend %s when no override is set", async (backend, expected) => {
    vi.resetModules();
    vi.stubEnv("VITE_BASE_URL", backend);
    vi.stubEnv("VITE_OUTCOMES_API_URL", undefined);

    const { OutcomesAPI } = await import("./OutcomesAPI");

    expect(OutcomesAPI.defaults.baseURL).toBe(expected);
  });

  it("honours an explicit API override", async () => {
    vi.resetModules();
    vi.stubEnv("VITE_BASE_URL", "http://localhost:9999");
    vi.stubEnv("VITE_OUTCOMES_API_URL", "https://govtool.example/outcomes/");

    const { OutcomesAPI } = await import("./OutcomesAPI");

    expect(OutcomesAPI.defaults.baseURL).toBe("https://govtool.example/outcomes");
  });
});
