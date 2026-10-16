import { MetadataValidationStatus } from "@models";
import { getMetadataDataMissingStatusTranslation } from "../getMetadataDataMissingStatusTranslation";

describe("getMetadataDataMissingStatusTranslation", () => {
  it("should return the correct translation for URL_NOT_FOUND status", () => {
    const translation = getMetadataDataMissingStatusTranslation(
      MetadataValidationStatus.URL_NOT_FOUND,
    );
    expect(translation).toBe("Data Missing");
  });

  it("should return the correct translation for INVALID_JSONLD status", () => {
    const translation = getMetadataDataMissingStatusTranslation(
      MetadataValidationStatus.INVALID_JSONLD,
    );
    expect(translation).toBe("Data Formatted Incorrectly");
  });

  it("should return the correct translation for INVALID_HASH status", () => {
    const translation = getMetadataDataMissingStatusTranslation(
      MetadataValidationStatus.INVALID_HASH,
    );
    expect(translation).toBe("Data Not Verifiable");
  });

  it.each([
    [MetadataValidationStatus.EXCEEDS_LIMIT, "Data Too Large"],
    [MetadataValidationStatus.URL_BLOCKED, "Data URL Blocked"],
    [MetadataValidationStatus.INTERNAL_ERROR, "Data Not Checked"],
  ])("should return its own translation for %s", (status, expected) => {
    expect(getMetadataDataMissingStatusTranslation(status)).toBe(expected);
  });

  it("should return the default translation for unknown status", () => {
    const translation = getMetadataDataMissingStatusTranslation(
      "UNKNOWN_STATUS" as MetadataValidationStatus,
    );
    expect(translation).toBe("Data Missing");
  });
});
