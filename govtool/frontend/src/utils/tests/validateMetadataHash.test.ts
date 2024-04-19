import { checkIsMissingGAMetadata } from "..";
import { postValidate } from "@services";
import { MetadataStandard, MetadataValidationStatus } from "@/models";
import { vi } from "vitest";

const url = "https://example.com";
const hash = "abcdefg";

vi.mock("@services");

describe("checkIsMissingGAMetadata", () => {
  it("returns false when url and hash are correct", async () => {
    // Mock postValidate to resolve with a status
    postValidate.mockResolvedValueOnce({
      status: false,
    });

    const result = await checkIsMissingGAMetadata({ url, hash });

    expect(result).toBe(false);
    expect(postValidate).toHaveBeenCalledWith({
      url,
      hash,
      standard: MetadataStandard.CIP108,
    });
  });

  it("returns a MetadataValidationStatus when postValidate resolves when status is invalid hash", async () => {
    postValidate.mockResolvedValueOnce({
      status: MetadataValidationStatus.INVALID_HASH,
    });

    const result = await checkIsMissingGAMetadata({ url, hash });

    expect(result).toBe(MetadataValidationStatus.INVALID_HASH);
    expect(postValidate).toHaveBeenCalledWith({
      url,
      hash,
      standard: MetadataStandard.CIP108,
    });
  });

  it("returns a MetadataValidationStatus when postValidate resolves when status is invalid json", async () => {
    postValidate.mockResolvedValueOnce({
      status: MetadataValidationStatus.INVALID_JSONLD,
    });

    const result = await checkIsMissingGAMetadata({ url, hash });

    expect(result).toBe(MetadataValidationStatus.INVALID_JSONLD);
    expect(postValidate).toHaveBeenCalledWith({
      url,
      hash,
      standard: MetadataStandard.CIP108,
    });
  });

  it("returns MetadataValidationStatus.URL_NOT_FOUND when postValidate throws an error", async () => {
    // Mock postValidate to throw an error
    postValidate.mockRejectedValueOnce(new Error("404 Not Found"));

    const result = await checkIsMissingGAMetadata({ url, hash });

    expect(result).toBe(MetadataValidationStatus.URL_NOT_FOUND);
    expect(postValidate).toHaveBeenCalledWith({
      url,
      hash,
      standard: MetadataStandard.CIP108,
    });
  });
});
