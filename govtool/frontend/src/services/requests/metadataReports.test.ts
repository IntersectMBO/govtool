import { beforeEach, describe, expect, it, vi } from "vitest";

import { API } from "../API";
import {
  getMetadataReport,
  getMetadataReports,
  getMetadataResolve,
  postMetadataRetry,
} from "./metadataReports";

vi.mock("../API", () => ({ API: { get: vi.fn(), post: vi.fn() } }));

const mockGet = vi.mocked(API.get);
const mockPost = vi.mocked(API.post);

const hash = "ab".repeat(32);
const url = "https://example.com/doc.json?a=1&b=2";

describe("metadata report requests", () => {
  beforeEach(() => {
    vi.resetAllMocks();
  });

  it("resolves an anchor with hash and url as query params", async () => {
    const result = { ok: true, hash, body: {}, fetchedAt: "t" };
    mockGet.mockResolvedValue({ status: 200, data: result });

    await expect(getMetadataResolve(hash, url)).resolves.toBe(result);
    expect(mockGet).toHaveBeenCalledWith("/metadata/resolve", {
      validateStatus: expect.any(Function),
      params: { hash, url },
    });
  });

  it("posts a retry with the anchor in the body", async () => {
    const outcome = { refetched: false, retryAfterSeconds: 42, result: {} };
    mockPost.mockResolvedValue({ status: 200, data: outcome });

    await expect(postMetadataRetry(hash, url)).resolves.toBe(outcome);
    expect(mockPost).toHaveBeenCalledWith(
      "/metadata/retry",
      { hash, url },
      { validateStatus: expect.any(Function) },
    );
  });

  it("answers null for a report the backend does not have", async () => {
    mockGet.mockResolvedValue({ status: 404, data: {} });
    await expect(getMetadataReport("r/1")).resolves.toBeNull();
    expect(mockGet).toHaveBeenCalledWith("/metadata/reports/r%2F1", {
      validateStatus: expect.any(Function),
    });
  });

  it("throws, rather than navigating away, when the service is unavailable", async () => {
    mockGet.mockResolvedValue({ status: 503, data: {} });
    await expect(getMetadataReports(hash, url)).rejects.toThrow("503");
    await expect(getMetadataResolve(hash, url)).rejects.toThrow("503");
  });

  it("lists reports and tolerates a non-array answer", async () => {
    const rows = [{ id: "r2" }, { id: "r1" }];
    mockGet.mockResolvedValueOnce({ status: 200, data: rows });
    await expect(getMetadataReports(hash, url)).resolves.toBe(rows);

    mockGet.mockResolvedValueOnce({ status: 200, data: "nope" });
    await expect(getMetadataReports(hash, url)).resolves.toEqual([]);
  });
});
