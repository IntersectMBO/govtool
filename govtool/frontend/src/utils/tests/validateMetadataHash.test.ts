import { vi } from "vitest";
import { API } from "@/services";
import { validateMetadataHash } from "..";

describe("validateMetadataHash", () => {
  const mockHash = "abcdef1234567890";

  it("should throw an error for invalid URL", async () => {
    const invalidURL = "invalid-url";
    await expect(validateMetadataHash(invalidURL, mockHash)).rejects.toThrow(
      "Invalid URL",
    );
  });

  it("should throw an error for invalid JSON", async () => {
    const invalidJSONURL = "https://example.com/invalid-json";
    vi.spyOn(API, "get").mockRejectedValueOnce(new Error("Invalid JSON"));
    await expect(
      validateMetadataHash(invalidJSONURL, mockHash),
    ).rejects.toThrow("Invalid JSON");
  });

  // TODO: Provide tests for invalid hash
});
