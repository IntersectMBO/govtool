import { describe, it, expect, vi, beforeEach } from "vitest";

import { API } from "../API";
import { getSystemFeatures } from "./getSystemFeatures";

vi.mock("../API", () => ({ API: { get: vi.fn() } }));

const mockGet = vi.mocked(API.get);

describe("getSystemFeatures", () => {
  beforeEach(() => {
    vi.resetAllMocks();
  });

  it("returns the feature set the backend derived", async () => {
    const featureSet = {
      provider: "koios",
      network: "preview",
      generatedAt: "2026-01-01T00:00:00Z",
      unavailable: {},
      options: {},
      caveats: [],
    };
    mockGet.mockResolvedValue({ status: 200, data: featureSet });

    await expect(getSystemFeatures()).resolves.toBe(featureSet);
    expect(mockGet).toHaveBeenCalledWith("/system/features", {
      validateStatus: expect.any(Function),
    });
  });

  it("keeps a server error away from the shared 500 interceptor", async () => {
    // That interceptor navigates the whole app to the error page on any 500.
    // Accepting every status is what stops an advisory fetch from doing that:
    // the failure surfaces as a thrown error the caller turns into
    // `capabilitiesStatus: "unavailable"`.
    mockGet.mockResolvedValue({ status: 500, data: undefined });

    await expect(getSystemFeatures()).rejects.toThrow(
      "/system/features responded 500",
    );

    const config = mockGet.mock.calls[0][1] as {
      validateStatus: (status: number) => boolean;
    };
    expect(config.validateStatus(500)).toBe(true);
    expect(config.validateStatus(404)).toBe(true);
  });

  it("rejects a body that is not a feature set", async () => {
    mockGet.mockResolvedValue({ status: 200, data: { schemaVersion: 1 } });

    // Rejecting is what keeps the UI gates failing OPEN: the caller marks
    // capabilities `unavailable` rather than gating on a shape it cannot read.
    await expect(getSystemFeatures()).rejects.toThrow(
      "did not answer with a feature set",
    );
  });

  it("rejects an empty body", async () => {
    mockGet.mockResolvedValue({ status: 200, data: undefined });

    await expect(getSystemFeatures()).rejects.toThrow(
      "did not answer with a feature set",
    );
  });
});
